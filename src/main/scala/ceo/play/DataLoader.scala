package ceo.play

import java.io.{BufferedReader, File, StringReader}

import ceo.play.PlayerTeam._

import scala.collection.mutable
import scala.io.Source
import scala.util.{Failure, Success, Try}

object DataLoader {

  private val pieces: mutable.Map[String, PieceData] =
    mutable.Map[String, PieceData]()
  private var piecesToCheck: List[String] = List[String]()

  def getAllPieceData: Iterable[PieceData] = pieces.values

  def clearPiecesToCheck(): Unit = {
    piecesToCheck = Nil
  }

  def getPieceData(name: String, team: PlayerTeam): PieceData = pieces(name + "_" + team)

  def main(args: Array[String]): Unit = {
    loadPieceFiles()
    println(initialize(boardFileName = "Data/boardTest.ceo", whitePlayerInBottom = true, showErrors = true)._1)
  }

  def loadPieceFiles(file: File = new File("Data/Units")): Unit = {
    if (pieces.isEmpty) {
      if (file.isDirectory)
        file.listFiles().filter(_.getName.endsWith(".ceo")).foreach(loadPieceFile)
      else
        new RuntimeException(s"Not a directory: $file")
    }
  }

  def loadPieceFile(file: File): Unit = {
    val br = new BufferedReader(new StringReader(Source.fromFile(file).getLines.mkString("\n")))

    var names: List[PieceData] = List.empty

    def loadUnits(): Unit = {
      val gamePieces = loadUnit(file.getName, br)
      gamePieces.foreach {
        gamePiece =>
          if (gamePiece.team == PlayerTeam.WhiteBottom) // we only need to add it once per piece name
            names = gamePiece :: names
          pieces.put(gamePiece.nameWithPlayerBase, gamePiece)
      }
      if (gamePieces.nonEmpty)
        loadUnits()
    }

    loadUnits()

    val namesRev = names.reverse
    val namesSorted = names.sortBy(_.nameWithTier)
    if (names.lengthCompare(names.distinct.size) != 0) {
      System.err.println(s"There are duplicated piece names in $file:")
      System.err.println(namesSorted.zip(namesSorted.distinct).find { case (a, b) => a != b }.get._1)
      ???
    }

    if (namesRev != namesSorted) {
      val list1 = namesRev.map(_.name)
      val list2 = namesSorted.map(_.name)
      val listIndex = list1.zip(list2).indexWhere { case (a, b) => a != b }
      System.err.println(s"Pieces not in alphabetic order in $file:")
      System.err.println(list1.drop(listIndex - 1))
      System.err.println("Should be:")
      System.err.println(list2.drop(listIndex - 1))
      ???
    }
  }

  def loadUnit(fileName: String, br: BufferedReader): Seq[PieceData] = {
    def readLine(ignoreEmptyLines: Boolean = true): String = {
      val line = br.readLine()
      if (line != null && (line.isEmpty && ignoreEmptyLines || line.startsWith("//"))) readLine() else line
    }

    val firstLine = readLine()
    if (firstLine == null)
      Seq.empty
    else {
      val List(pieceName, morale) = firstLine.split(" ").toList
      if ("[A-Z][a-zA-Z\\-]+(\\+){0,3}".r.findAllIn(pieceName).isEmpty)
        throw new Exception(s"Piece name is not valid: '$pieceName' in $fileName")

      br.readLine()
      val movesStr = Stream.continually(readLine(false)).takeWhile(!_.startsWith("-" * 3)).toList
      val powersStr = Stream.continually(readLine()).takeWhile(!_.startsWith("-" * 20)).toList

      val powers1 = loadPowers(powersStr)
      val (movesBottomTop1, extraPowersBottomTop) = loadMoves(pieceName, movesStr, powers1)
      val (movesTopBottom1, extraPowersTopBottom) = loadMoves(pieceName, movesStr.reverse, powers1)

      val augmentedMoves = powers1.collect {
        case power: AugmentedMovePower => power.createMoves
      }.flatten

      val powers2 = powers1.filter {
        case _: AugmentedMovePower => false
        case _ => true
      }

      val movesBottomTop2 = movesBottomTop1 ++ augmentedMoves
      val movesTopBottom2 = movesTopBottom1 ++ augmentedMoves

      val powersBottomTop = powers2 ++ extraPowersBottomTop
      val powersTopBottom = powers2 ++ extraPowersTopBottom

      val nameWhite = pieceName + "_" + PlayerColor.White
      val nameBlack = pieceName + "_" + PlayerColor.Black
      val isMinion = fileName.startsWith("minions")
      val isChampion = fileName.startsWith("champions") && !pieceName.startsWith("King")
      val isExtra = fileName.endsWith("extra")
      Seq(
        PieceData(nameWhite, isMinion, isChampion, isExtra, morale.toInt, movesBottomTop2, powersBottomTop, WhiteBottom),
        PieceData(nameWhite, isMinion, isChampion, isExtra, morale.toInt, movesTopBottom2, powersTopBottom, WhiteTop),
        PieceData(nameBlack, isMinion, isChampion, isExtra, morale.toInt, movesBottomTop2, powersBottomTop, BlackBottom),
        PieceData(nameBlack, isMinion, isChampion, isExtra, morale.toInt, movesTopBottom2, powersTopBottom, BlackTop)
      )
    }
  }

  def loadMoves(pieceName: String, movesStr: List[String], powers: List[Powers]): (List[Moves], List[Powers]) = {
    val iy = movesStr.indexWhere(_.contains('@'))
    if (iy == -1)
      throw new Exception(s"$pieceName moves needs to contain @ character to define piece position.")
    val ix = movesStr(iy).indexWhere(_ == '@')

    var completePos = List[(Char, Distance)]()
    var positionalPowerPos = List[(Char, Distance)]()
    var maybePositionalPower = Option.empty[PositionalPower]
    var positionsToBlockAttacks = List[Distance]()

    val simpleMoves: List[Moves] =
      for {
        (line, y) <- movesStr.map(_.stripPrefix("-")).zipWithIndex
        (char, x) <- line.zipWithIndex
        if char != ' ' && char != '@'
        dist = Distance(y - iy, x - ix)
      } yield char match {
        case 'N' => Moves.MoveOrAttack(dist)
        case 'M' => Moves.Move(dist)
        case 'm' => Moves.MoveFromStart(dist)
        case 'T' => Moves.MoveUnblockable(dist)
        case 't' => Moves.MoveUnblockableFromStart(dist)
        case 'A' => Moves.Attack(dist)
        case 'J' => Moves.MoveOrAttackUnblockable(dist)
        case 'S' => Moves.MoveOrAttackOrSwapAlly(dist)
        case 'P' => Moves.MoveOrSwapAlly(dist)
        case 'R' => Moves.RangedDestroy(dist)
        case 'B' =>
          positionsToBlockAttacks = dist :: positionsToBlockAttacks
          Moves.MoveOrAttack(dist)
        case '1' | '2' | '3' | '4' =>
          powers.collectFirst {
            case move: MovePower if move.letterOfMove == char => move
            case move: PositionalPower if move.letterOfMove == char => move
            case move: MovePowerComplete if move.lettersOfMoves.contains(char) => move
          } match {
            case None =>
              throw new Exception("Unknown 'MovePower' letter: " + char)
            case Some(movePower: MovePower) =>
              movePower.createMove(dist)
            case Some(positionalPower: PositionalPower) =>
              positionalPowerPos = (char, dist) :: positionalPowerPos
              maybePositionalPower = Some(positionalPower)
              Moves.Empty
            case Some(_: MovePowerComplete) =>
              completePos = (char, dist) :: completePos
              Moves.Empty
            case _ =>
              ???
          }
      }

    val moves: List[Moves] =
      simpleMoves.filterNot(_ == Moves.Empty) ++
        powers.collect { case move: MovePowerComplete => move }.flatMap {
          _.createMoves(completePos.groupBy(_._1).mapValues(_.map(_._2)))
        }

    val extraPowers: List[Powers] =
      maybePositionalPower.map {
        positionalPower =>
          positionalPower.createPowers(positionalPowerPos.groupBy(_._1).mapValues(_.map(_._2)))
      }.getOrElse(List.empty[Powers]) ++ {
        if (positionsToBlockAttacks.isEmpty)
          List.empty
        else
          List(Powers.BlockAttacksFrom(positionsToBlockAttacks.toSet))
      }

    (moves, extraPowers)
  }

  def loadPowers(powersStr: List[String]): List[Powers] = {
    def getLetter(str: String): Char = {
      assume(str.length == 1)
      str.head
    }

    powersStr.map {
      // 0-arg Powers:
      case "KingCastling" =>
        Powers.KingCastlingPower
      case "OnAnyKillSuicide" =>
        Powers.OnAnyKillSuicides
      case "GhostMovement" =>
        Powers.GhostMovement
      case "OnMeleeDeathKillAttacker" =>
        Powers.OnMeleeDeathKillAttacker
      case "OnKillMercenary" =>
        Powers.OnKillMercenary
      case "CanOnlyActAfterPieceLost" =>
        Powers.CanOnlyActAfterPieceLost
      case "OnMagicVanish" =>
        Powers.OnMagicVanish
      case "OnEnemyDeathMovesForward" =>
        Powers.OnEnemyDeathMovesForward
      case "OnMoveAdjacentHoplitesMove" =>
        Powers.OnMoveAdjacentHoplitesMove
      case "CannotBeTargetedByMinions" =>
        Powers.CannotBeTargetedByMinions
      case "WispReflect" =>
        Powers.WispReflect
      case "OnChampionKillSwapEnemyKing" =>
        Powers.OnChampionKillSwapEnemyKing
      case "Dummy" =>
        Powers.Dummy
      case "OnKillGainHalfMorale" =>
        Powers.OnKillGainHalfMorale
      case "OnDeathHalfMoraleToKing" =>
        Powers.OnDeathHalfMoraleToKing
      // 1-arg Powers:
      case str if str.startsWith("DummyNothingPower ") =>
        Powers.DummyNothingPower(getLetter(str.drop("DummyNothingPower ".length)))
      case str if str.startsWith("PromotesTo ") =>
        val pieceToCheck = str.drop("PromotesTo ".length)
        piecesToCheck = pieceToCheck :: piecesToCheck
        Powers.PromoteTo(pieceToCheck)
      case str if str.startsWith("OnAnyDeathPlayerChangeMorale ") =>
        Powers.OnAnyDeathPlayerChangeMorale(str.drop("OnAnyDeathPlayerChangeMorale ".length).toInt)
      case str if str.startsWith("OnKillPieceGainMorale ") =>
        Powers.OnKillPieceGainMorale(str.drop("OnKillPieceGainMorale ".length).toInt)
      case str if str.startsWith("OnKillPlayerChangeMorale ") =>
        Powers.OnKillPlayerChangeMorale(str.drop("OnKillPlayerChangeMorale ".length).toInt)
      case str if str.startsWith("OnKillTransformInto ") =>
        val pieceToCheck = str.drop("OnKillTransformInto ".length)
        piecesToCheck = pieceToCheck :: piecesToCheck
        Powers.OnKillTransformInto(pieceToCheck)
      case str if str.startsWith("OnMagicCastPromoteTo ") =>
        val pieceToCheck = str.drop("OnMagicCastPromoteTo ".length)
        piecesToCheck = pieceToCheck :: piecesToCheck
        Powers.OnMagicCastPromoteTo(pieceToCheck)
      case str if str.startsWith("TriggerWrathOnAdjacentAllyDeath ") =>
        Powers.TriggerWrathOnAdjacentAllyDeath(str.drop("TriggerWrathOnAdjacentAllyDeath ".length).toInt)
      case str if str.startsWith("BeginsGameEnchanted ") =>
        Powers.BeginsGameEnchanted(str.drop("BeginsGameEnchanted ".length).toInt)
      case str if str.startsWith("OnMagicCastDecayDeath ") =>
        Powers.OnMagicCastDecayDeath(str.drop("OnMagicCastDecayDeath ".length).toInt)
      case str if str.startsWith("OnDeathEnchantAdjacentChampions ") =>
        Powers.OnDeathEnchantAdjacentChampions(str.drop("OnDeathEnchantAdjacentChampions ".length).toInt)
      case str if str.startsWith("OnDeathEnchantGlobalMinions ") =>
        Powers.OnDeathEnchantGlobalMinions(str.drop("OnDeathEnchantGlobalMinions ".length).toInt)
      case str if str.startsWith("OnDeathHostageChangesMorale ") =>
        Powers.OnDeathHostageChangesMorale(str.drop("OnDeathHostageChangesMorale ".length).toInt)
      case str if str.startsWith("HostageCaught ") =>
        Powers.HostageCaught(str.drop("HostageCaught ".length).toInt)
      case str if str.startsWith("OnDeathPhoenix ") =>
        val pieceName = str.drop("OnDeathPhoenix ".length)
        piecesToCheck = pieceName :: piecesToCheck
        Powers.OnDeathPhoenix(pieceName)
      case str if str.startsWith("OnKillPromoteToKing ") =>
        Powers.OnKillPromoteToKing(str.drop("OnKillPromoteToKing ".length).toInt)
      case str if str.startsWith("OnMagicCastPromoteIfEnemy ") =>
        Powers.OnMagicCastPromoteIfEnemy(str.drop("OnMagicCastPromoteIfEnemy ".length))
      case str if str.startsWith("OnAllyDeathPieceChangeMorale ") =>
        Powers.OnAllyDeathPieceChangeMorale(str.drop("OnAllyDeathPieceChangeMorale ".length).toInt)
      case str if str.startsWith("TriggerFrostMephit ") =>
        Powers.TriggerFrostMephit(str.drop("TriggerFrostMephit ".length).toDouble)
      // Multiple-arg Powers:
      case str if str.startsWith("DecayAfterTurn ") =>
        val List(turnStarts, moralePerTurn) = str.drop("DecayAfterTurn ".length).split(" ").toList
        Powers.DecayAfterTurn(turnStarts.toInt, moralePerTurn.toInt)
      case str if str.startsWith("ImmuneTo ") =>
        val list = str.drop("ImmuneTo ".length).split(" ").toList.flatMap(effect =>
          if (effect == "Status") List(EffectType.Petrify, EffectType.Freeze, EffectType.Poison).map(_.name) else List(effect))
        Powers.ImmuneTo(list.map(EffectType.apply))
      case str if str.startsWith("DestroyedBy ") =>
        Powers.DestroyedBy(str.drop("DestroyedBy ".length).split(" ").toList.map(EffectType.apply))
      case str if str.startsWith("OnKillVampireAbility ") =>
        val List(moraleTakenFromEnemy, moraleToKing) = str.drop("OnKillVampireAbility ".length).split(" ").toList
        Powers.OnKillVampireAbility(moraleTakenFromEnemy.toInt, moraleToKing.toInt)
      case str if str.startsWith("OnMagicCastDecayTo ") =>
        val List(decayAmount, limitToDevolve, pieceName) = str.drop("OnMagicCastDecayTo ".length).split(" ").toList
        piecesToCheck = pieceName :: piecesToCheck
        Powers.OnMagicCastDecayTo(decayAmount.toInt, limitToDevolve.toInt, pieceName)
      case str if str.startsWith("GrowMoraleOnPlayerTurnUntilTransform ") =>
        val List(moraleToPromote, pieceName) = str.drop("GrowMoraleOnPlayerTurnUntilTransform ".length).split(" ").toList
        piecesToCheck = pieceName :: piecesToCheck
        Powers.GrowMoraleOnPlayerTurnUntilTransform(moraleToPromote.toInt, pieceName)
      case str if str.startsWith("GrowMoraleUntilTransform ") =>
        val List(moraleToPromote, pieceName) = str.drop("GrowMoraleUntilTransform ".length).split(" ").toList
        piecesToCheck = pieceName :: piecesToCheck
        Powers.GrowMoraleUntilTransform(moraleToPromote.toInt, pieceName)
      case str if str.startsWith("OnMeleeDeathPoisonIfMoraleLess ") =>
        val List(maxMoraleToPoison, turnsToDeath) = str.drop("OnMeleeDeathPoisonIfMoraleLess ".length).split(" ").toList
        Powers.OnMeleeDeathPoisonIfMoraleLess(maxMoraleToPoison.toInt, turnsToDeath.toInt)
      case str if str.startsWith("OnKillDecayTo ") =>
        val List(moraleLostOnKill, moraleLimit, pieceName) = str.drop("OnKillDecayTo ".length).split(" ").toList
        piecesToCheck = pieceName :: piecesToCheck
        Powers.OnKillDecayTo(moraleLostOnKill.toInt, moraleLimit.toInt, pieceName)
      case str if str.startsWith("OnGlobalDeathGainValueUntil ") =>
        val List(moraleGainOnGlobalDeath, moraleLimit, pieceName) = str.drop("OnGlobalDeathGainValueUntil ".length).split(" ").toList
        piecesToCheck = pieceName :: piecesToCheck
        Powers.OnGlobalDeathGainValueUntil(moraleGainOnGlobalDeath.toInt, moraleLimit.toInt, pieceName)
      case str if str.startsWith("OnDeathAdjacentEnemiesFreezePush ") =>
        val List(pushDistance, freezeDuration) = str.drop("OnDeathAdjacentEnemiesFreezePush ".length).split(" ").toList
        Powers.OnDeathAdjacentEnemiesFreezePush(pushDistance.toInt, freezeDuration.toInt)
      // Move Powers:
      case str if str.startsWith("MagicDestroy ") =>
        Powers.MagicDestroyMovePower(getLetter(str.drop("MagicDestroy ".length)))
      case str if str.startsWith("RangedPetrify ") =>
        val List(letterStr, duration) = str.drop("RangedPetrify ".length).split(" ").toList
        Powers.RangedPetrifyMovePower(getLetter(letterStr), duration.toInt)
      case str if str.startsWith("WeakRangedPetrify ") =>
        val List(letterStr, duration) = str.drop("WeakRangedPetrify ".length).split(" ").toList
        Powers.WeakRangedPetrifyMovePower(getLetter(letterStr), duration.toInt)
      case str if str.startsWith("MagicPoison ") =>
        val List(letterStr, duration) = str.drop("MagicPoison ".length).split(" ").toList
        Powers.MagicPoisonMovePower(getLetter(letterStr), duration.toInt)
      case str if str.startsWith("TaurusRush ") =>
        val List(letterStr, maxDistance) = str.drop("TaurusRush ".length).split(" ").toList
        Powers.TaurusRushMovePower(getLetter(letterStr), maxDistance.toInt)
      case str if str.startsWith("MagicTransformIntoAlly ") =>
        val List(letterStr, moraleCost, allyUnitName) = str.drop("MagicTransformIntoAlly ".length).split(" ").toList
        piecesToCheck = allyUnitName :: piecesToCheck
        Powers.TransformIntoAllyMovePower(getLetter(letterStr), moraleCost.toInt, allyUnitName)
      case str if str.startsWith("MagicSummonOrTransformIntoAlly ") =>
        val List(letterStr, moraleCost, allyUnitName) = str.drop("MagicSummonOrTransformIntoAlly ".length).split(" ").toList
        piecesToCheck = allyUnitName :: piecesToCheck
        Powers.SummonOrTransformIntoAllyMovePower(getLetter(letterStr), moraleCost.toInt, allyUnitName)
      case str if str.startsWith("JumpMinion ") =>
        Powers.JumpMinionMovePower(getLetter(str.drop("JumpMinion ".length)))
      case str if str.startsWith("MagicCharmMinion ") =>
        Powers.MagicCharmMinionMovePower(getLetter(str.drop("MagicCharmMinion ".length)))
      case str if str.startsWith("RangedPush ") =>
        val List(letterStr, moraleCost, pushDistance) = str.drop("RangedPush ".length).split(" ").toList
        Powers.RangedPushMovePower(getLetter(letterStr), moraleCost.toInt, pushDistance.toInt)
      case str if str.startsWith("MagicPushFreeze ") =>
        val List(letterStr, maxPushDistance, freezeDuration) = str.drop("MagicPushFreeze ".length).split(" ").toList
        Powers.MagicPushFreezeMovePower(getLetter(letterStr), maxPushDistance.toInt, freezeDuration.toInt)
      case str if str.startsWith("MagicFreeze ") =>
        val List(letterStr, freezeDuration) = str.drop("MagicFreeze ".length).split(" ").toList
        Powers.MagicFreezeMovePower(getLetter(letterStr), freezeDuration.toInt)
      case str if str.startsWith("MagicLightningOnLocation ") =>
        val List(letterStr, moraleCost, lightningDelayTurns) = str.drop("MagicLightningOnLocation ".length).split(" ").toList
        Powers.MagicLightningOnLocationMovePower(getLetter(letterStr), moraleCost.toInt, lightningDelayTurns.toInt)
      case str if str.startsWith("UnstoppableTeleportTransformInto ") =>
        val List(letterStr, pieceName) = str.drop("UnstoppableTeleportTransformInto ".length).split(" ").toList
        piecesToCheck = pieceName :: piecesToCheck
        Powers.UnstoppableTeleportTransformIntoMovePower(getLetter(letterStr), pieceName)
      case str if str.startsWith("MagicStonePillar ") =>
        val List(letterStr, moraleCost, durationTurns) = str.drop("MagicStonePillar ".length).split(" ").toList
        Powers.MagicStonePillarMovePower(getLetter(letterStr), moraleCost.toInt, durationTurns.toInt)
      case str if str.startsWith("AugmentedTeleportBeacon ") =>
        val List(letterStr, augmentedRange) = str.drop("AugmentedTeleportBeacon ".length).split(" ").toList
        Powers.TeleportBeaconMovePower(getLetter(letterStr), augmentedRange.toInt)
      case str if str.startsWith("RangedSummonGeminiTwin ") =>
        val List(letterStr, moraleCost, pieceName) = str.drop("RangedSummonGeminiTwin ".length).split(" ").toList
        piecesToCheck = pieceName :: piecesToCheck
        Powers.RangedSummonGeminiTwinMovePower(getLetter(letterStr), moraleCost.toInt, pieceName)
      case str if str.startsWith("MagicWeakEnchant ") =>
        val List(letterStr, durationTurns) = str.drop("MagicWeakEnchant ".length).split(" ").toList
        Powers.MagicWeakEnchantMovePower(getLetter(letterStr), durationTurns.toInt)
      case str if str.startsWith("MagicEnvyClone ") =>
        Powers.MagicEnvyCloneMovePower(getLetter(str.drop("MagicEnvyClone ".length)))
      case str if str.startsWith("MagicMeteor ") =>
        val List(letterStr, moraleCost, turnsToMeteor) = str.drop("MagicMeteor ".length).split(" ").toList
        Powers.MagicMeteorMovePower(getLetter(letterStr), moraleCost.toInt, turnsToMeteor.toInt)
      case str if str.startsWith("MagicPush ") =>
        val List(letterStr, moraleCost, pushDistance) = str.drop("MagicPush ".length).split(" ").toList
        Powers.MagicPushMovePower(getLetter(letterStr), moraleCost.toInt, pushDistance.toInt)
      case str if str.startsWith("MagicSummonPiece ") =>
        val List(letterStr, moraleCost, pieceName) = str.drop("MagicSummonPiece ".length).split(" ").toList
        piecesToCheck = pieceName :: piecesToCheck
        Powers.MagicSummonPieceMovePower(getLetter(letterStr), moraleCost.toInt, pieceName)
      case str if str.startsWith("TemperanceAttack ") =>
        Powers.TemperanceAttackUnblockableMovePower(getLetter(str.drop("TemperanceAttack ".length)))
      case str if str.startsWith("TemperanceAttackOrMove ") =>
        Powers.TemperanceAttackOrMoveMovePower(getLetter(str.drop("TemperanceAttackOrMove ".length)))
      case str if str.startsWith("RangedCompel ") =>
        val List(letterStr, turnsCompelled) = str.drop("RangedCompel ".length).split(" ").toList
        Powers.RangedCompelMovePower(getLetter(letterStr), turnsCompelled.toInt)
      case str if str.startsWith("MagicNecromancerSkeleton ") =>
        val List(letterStr, moraleCost) = str.drop("MagicNecromancerSkeleton ".length).split(" ").toList
        Powers.MagicNecromancerSkeletonMovePower(getLetter(letterStr), moraleCost.toInt)
      case str if str.startsWith("MagicDestroySelfAquariusAt ") =>
        val List(letterStr, freezeDuration) = str.drop("MagicDestroySelfAquariusAt ".length).split(" ").toList
        Powers.MagicDestroySelfAquariusAtMovePower(getLetter(letterStr), freezeDuration.toInt)
      case str if str.startsWith("MagicDestroySelfButterfly ") =>
        val List(letterStr, turnsDelay, turnsEnchanted) = str.drop("MagicDestroySelfButterfly ".length).split(" ").toList
        Powers.MagicDestroySelfButterflyMovePower(getLetter(letterStr), turnsDelay.toInt, turnsEnchanted.toInt)
      // Move Power Complete:
      case str if str.startsWith("TeleportManyToOne ") =>
        Powers.TeleportManyToOneMovePowerComplete(str.drop("TeleportManyToOne ".length).split(" ").toList.map(getLetter))
      case str if str.startsWith("MagicMoveTargetTowards ") =>
        val List(from, towards, moraleCost) = str.drop("MagicMoveTargetTowards ".length).split(" ").toList
        Powers.MagicMoveTargetTowardsMovePowerComplete(getLetter(from), getLetter(towards), moraleCost.toInt)
      case str if str.startsWith("TeleportOneToMany ") =>
        Powers.TeleportOneToManyMovePowerComplete(str.drop("TeleportOneToMany ".length).split(" ").toList.map(getLetter))
      case str if str.startsWith("TeleportKingToLocation ") =>
        Powers.TeleportKingToLocationMovePowerComplete(str.drop("TeleportKingToLocation ".length).split(" ").toList.map(getLetter))
      case str if str.startsWith("PatienceCannotAttackBeforeTurn ") =>
        val List(moveOrAttack, attack, untilTurn) = str.drop("PatienceCannotAttackBeforeTurn ".length).split(" ").toList
        Powers.PatienceCannotAttackBeforeTurnMovePowerComplete(List(getLetter(moveOrAttack), getLetter(attack)), untilTurn.toInt)
      case str if str.startsWith("RangedPushSpawn ") =>
        val List(letterStr, moraleCost, pushDistance, pieceName) = str.drop("RangedPushSpawn ".length).split(" ").toList
        piecesToCheck = pieceName :: piecesToCheck
        Powers.RangedPushSpawnMovePower(getLetter(letterStr), moraleCost.toInt, pushDistance.toInt, pieceName)
      case str if str.startsWith("Chastity ") =>
        Powers.ChastityMovePowerComplete(str.drop("Chastity ".length).split(" ").toList.map(getLetter))
      case str if str.startsWith("AugmentedTeleportRoyalGuard ") =>
        Powers.AugmentedTeleportRoyalGuardPowerComplete(str.drop("AugmentedTeleportRoyalGuard ".length).split(" ").toList.map(getLetter))
      // Positional Powers:
      case str if str.startsWith("OnMeleeDeathSpawnSlimes ") =>
        val List(letterStr, pieceToCheck) = str.drop("OnMeleeDeathSpawnSlimes ".length).split(" ").toList
        piecesToCheck = pieceToCheck :: piecesToCheck
        Powers.OnMeleeDeathSpawnSlimesPositionalPower(getLetter(letterStr), pieceToCheck)
      case str if str.startsWith("OnMeleeDeathKillAttackerPosition ") =>
        Powers.OnMeleeDeathKillAttackerPositionalPower(getLetter(str.drop("OnMeleeDeathKillAttackerPosition ".length)))
      case str if str.startsWith("TriggerGuardian ") =>
        Powers.TriggerGuardianPositionalPower(getLetter(str.drop("TriggerGuardian ".length)))
      case str if str.startsWith("TriggerInstantKill ") =>
        Powers.TriggerInstantKillPositionalPower(getLetter(str.drop("TriggerInstantKill ".length)))
      case str if str.startsWith("OnMeleeDeathTriggerRevive ") =>
        val List(letterStr, moraleMinimum) = str.drop("OnMeleeDeathTriggerRevive ".length).split(" ").toList
        Powers.OnMeleeDeathTriggerRevivePositionalPower(getLetter(letterStr), moraleMinimum.toInt)
      case str if str.startsWith("OnDeathTriggerFreezeMinions ") =>
        val List(letterStr, freezeDuration) = str.drop("OnDeathTriggerFreezeMinions ".length).split(" ").toList
        Powers.OnDeathTriggerFreezeMinionsPositionalPower(getLetter(letterStr), freezeDuration.toInt)
      case str if str.startsWith("MagicTriggerLust ") =>
        Powers.MagicTriggerLustPositionalPower(getLetter(str.drop("MagicTriggerLust ".length)))
      // Augmented Move Powers:
      case "AugmentedTeleportGhast" =>
        Powers.AugmentedTeleportGhastMovePower
      case str if str.startsWith("MagicFreezeStrikeGlobalEnemyChampion ") =>
        Powers.MagicFreezeStrikeGlobalEnemyChampion(str.drop("MagicFreezeStrikeGlobalEnemyChampion ".length).toInt)
      case str =>
        throw new Exception("Unknown Power: " + str)
    }
  }

  def initialize(boardFileName: String, whitePlayerInBottom: Boolean, showErrors: Boolean): (GameState, List[String]) = {
    initialize(new File(boardFileName), whitePlayerInBottom, showErrors)
  }

  def initialize(boardFile: File, whitePlayerInBottom: Boolean, showErrors: Boolean): (GameState, List[String]) = {
    val lines = Source.fromFile(boardFile).getLines.toVector
    if (lines.size == 9 && lines(8).nonEmpty || lines.size > 9)
      throw new Exception(s"file $boardFile has more than 8 lines!")

    val board = loadBoard(lines, whitePlayerInBottom)

    val unknownPieces =
      piecesToCheck.flatMap(piece => if (Try(getPieceData(piece, WhiteBottom)).isFailure) Some(piece) else None)
        .distinct
        .sorted

    if (unknownPieces.nonEmpty && showErrors) {
      System.err.println("Unknown pieces:")
      System.err.println(unknownPieces.distinct.zipWithIndex.map {
        case (piece, index) => s"${index + 1}) $piece"
      }.mkString("\n"))
      ???
    } else
      (board, unknownPieces)
  }

  def loadBoard(lines: Seq[String], whitePlayerInBottom: Boolean): GameState = {
    var gameState = PlayGame.emptyGameState(whitePlayerInBottom)
    for (row <- lines.indices) {
      val line = lines(row).replaceAll("""\s+""", " ")
      val pieceNames = line.split(" ")
      for ((pieceName, column) <- pieceNames.zipWithIndex) {
        if (pieceName.length == 1 && pieceName != "e")
          gameState = gameState.placePiece(PieceData.empty.copy(name = pieceName).createPiece(BoardPos(row, column)))
        else if (pieceName.length > 1) {
          val List(name, team) = pieceName.split("_").toList
          Try(getPieceData(name, PlayerTeam(team, whitePlayerInBottom))) match {
            case Failure(_) =>
              piecesToCheck = name :: piecesToCheck
              gameState = gameState.placePiece(PieceData.empty.copy(name = "?" + pieceName).createPiece(BoardPos(row, column)))
            case Success(pieceData) =>
              gameState = gameState.placePiece(pieceData.createPiece(BoardPos(row, column)))
          }
        }
      }
    }

    gameState.playerWhite.allPieces.foreach {
      piece =>
        gameState = gameState
          .updatePlayer(gameState.playerWhite.updateGuardedPositions(None, Some(piece)))
    }

    gameState.playerBlack.allPieces.foreach {
      piece =>
        gameState = gameState
          .updatePlayer(gameState.playerBlack.updateGuardedPositions(None, Some(piece)))
    }

    gameState
      .updatePlayer(PlayerColor.White, _.optimizeRunners(gameState))
      .updatePlayer(PlayerColor.Black, _.optimizeRunners(gameState))
  }
}
