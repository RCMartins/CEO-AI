package ceo.play

import java.io.{BufferedReader, File, StringReader}

import ceo.play.PlayerTeam.{Black, White}

import scala.collection.mutable
import scala.io.Source
import scala.util.{Failure, Success, Try}

object DataLoader {

  private val pieces: mutable.Map[String, PieceData] = mutable.Map[String, PieceData]()
  private var piecesToCheck: List[String] = List[String]()

  def getPieceData(name: String, team: PlayerTeam): PieceData = pieces(name + "_" + team)

  def main(args: Array[String]): Unit = {
    println(initialize())
  }

  def initialize(boardToStart: String = "Data/boardTest.ceo"): GameState = {
    loadFiles(new File("Data/Units"))
    val board = loadBoard(new File(boardToStart))

    val unknownPieces =
      piecesToCheck.flatMap(piece => if (Try(getPieceData(piece, White)).isFailure) Some(piece) else None) ++
        piecesToCheck.flatMap(piece => if (Try(getPieceData(piece, Black)).isFailure) Some(piece) else None)

    if (unknownPieces.nonEmpty) {
      System.err.println("Unknown pieces:")
      System.err.println(unknownPieces.distinct.zipWithIndex.map {
        case (piece, index) => s"${index + 1}) $piece"
      }.mkString("\n"))
      ???
    } else
      board
  }

  def loadFiles(file: File): Unit = {
    if (file.isDirectory)
      file.listFiles().filter(_.getName.endsWith(".ceo")).foreach(loadFile)
    else
      new RuntimeException(s"Not a directory: $file")
  }

  def loadFile(file: File): Unit = {
    val br = new BufferedReader(new StringReader(Source.fromFile(file).getLines.mkString("\n")))

    var names: List[PieceData] = List.empty

    def loadUnits(): Unit = {
      val gamePieces = loadUnit(file.getName, br)
      gamePieces.foreach {
        gamePiece =>
          if (gamePiece.team == White)
            names = gamePiece :: names
          pieces.put(gamePiece.name, gamePiece)
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

      val powers = loadPowers(powersStr)
      val (movesBottomTop, extraPowersWhite) = loadMoves(pieceName, movesStr, powers)
      val (movesTopBottom, extraPowersBlack) = loadMoves(pieceName, movesStr.reverse, powers)

      val name_white = pieceName + "_" + White
      val name_black = pieceName + "_" + Black
      val isMinion = fileName.startsWith("minions")
      Seq(
        PieceData(name_white, isMinion, morale.toInt, movesBottomTop, powers ++ extraPowersWhite, White),
        PieceData(name_black, isMinion, morale.toInt, movesTopBottom, powers ++ extraPowersBlack, Black)
      )
    }
  }

  def loadMoves(pieceName: String, movesStr: List[String], powers: List[Powers]): (List[Moves], List[Powers]) = {
    val iy = movesStr.indexWhere(_.contains('@'))
    if (iy == -1)
      throw new Exception(s"$pieceName moves needs to contain @ character to define piece position.")
    val ix = movesStr(iy).indexWhere(_ == '@')

    var completePos = List[(Char, Distance)]()
    var maybeCompleteMovePower = Option.empty[MovePowerComplete]
    var positionalPowerPos = List[(Char, Distance)]()
    var maybePositionalPower = Option.empty[PositionalPower]

    val simpleMoves: List[Moves] =
      for {
        (line, y) <- movesStr.zipWithIndex
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
            case Some(movePowerComplete: MovePowerComplete) =>
              completePos = (char, dist) :: completePos
              maybeCompleteMovePower = Some(movePowerComplete)
              Moves.Empty
            case _ =>
              ???
          }
      }

    val moves: List[Moves] =
      simpleMoves.filterNot(_ == Moves.Empty) ++
        maybeCompleteMovePower.map {
          completeMovePower =>
            completeMovePower.createMoves(completePos.groupBy(_._1).mapValues(_.map(_._2)))
        }.getOrElse(List.empty[Moves])

    val extraPowers: List[Powers] =
      maybePositionalPower.map {
        positionalPower =>
          positionalPower.createPowers(positionalPowerPos.groupBy(_._1).mapValues(_.map(_._2)))
      }.getOrElse(List.empty[Powers])

    (moves, extraPowers)
  }

  def loadPowers(powersStr: List[String]): List[Powers] = {
    def getLetter(str: String): Char = {
      assume(str.length == 1)
      str.head
    }

    powersStr.map {
      // 0-arg Powers:
      case "SuicideOnKill" =>
        Powers.OnKillSuicide
      case "GhostMovement" =>
        Powers.GhostMovement
      case "OnMeleeDeathKillAttacker" =>
        Powers.OnMeleeDeathKillAttacker
      case "StatusImmune" =>
        Powers.ImmuneTo(EffectType.all)
      case "OnKillMercenary" =>
        Powers.OnKillMercenary
      // 1-arg Powers:
      case str if str.startsWith("DummyNothingPower ") =>
        Powers.DummyNothingPower(getLetter(str.drop("DummyNothingPower ".length)))
      case str if str.startsWith("PromotesTo ") =>
        val pieceToCheck = str.drop("PromotesTo ".length)
        piecesToCheck = pieceToCheck :: piecesToCheck
        Powers.PromoteTo(pieceToCheck)
      case str if str.startsWith("PlayerChangeMoraleOnDeath ") =>
        Powers.PlayerChangeMoraleOnDeath(str.drop("PlayerChangeMoraleOnDeath ".length).toInt)
      case str if str.startsWith("PieceChangeMoraleOnKill ") =>
        Powers.PieceChangeMoraleOnKill(str.drop("PieceChangeMoraleOnKill ".length).toInt)
      case str if str.startsWith("PlayerChangeMoraleOnKill ") =>
        Powers.PlayerChangeMoraleOnKill(str.drop("PlayerChangeMoraleOnKill ".length).toInt)
      case str if str.startsWith("OnKillTransformInto ") =>
        val pieceToCheck = str.drop("OnKillTransformInto ".length)
        piecesToCheck = pieceToCheck :: piecesToCheck
        Powers.OnKillTransformInto(pieceToCheck)
      case str if str.startsWith("OnSpellPromoteTo ") =>
        val pieceToCheck = str.drop("OnSpellPromoteTo ".length)
        piecesToCheck = pieceToCheck :: piecesToCheck
        Powers.OnSpellPromoteTo(pieceToCheck)
      // Multiple-arg Powers:
      case str if str.startsWith("DecayAfterTurn ") =>
        val List(turnStarts, moralePerTurn) = str.drop("DecayAfterTurn ".length).split(" ").toList
        Powers.DecayAfterTurn(turnStarts.toInt, moralePerTurn.toInt)
      case str if str.startsWith("ImmuneTo ") =>
        Powers.ImmuneTo(str.drop("ImmuneTo ".length).split(" ").toList.map(EffectType.apply))
      case str if str.startsWith("DestroyedBy ") =>
        Powers.DestroyedBy(str.drop("DestroyedBy ".length).split(" ").toList.map(EffectType.apply))
      // Move Powers:
      case str if str.startsWith("MagicDestroy ") =>
        Powers.MagicDestroyMovePower(getLetter(str.drop("MagicDestroy ".length)))
      case str if str.startsWith("RangedPetrify ") =>
        val List(letterStr, duration) = str.drop("RangedPetrify ".length).split(" ").toList
        Powers.RangedPetrifyMovePower(getLetter(letterStr), duration.toInt)
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
      case str if str.startsWith("JumpMinion ") =>
        Powers.JumpMinionMovePower(getLetter(str.drop("JumpMinion ".length)))
      case str if str.startsWith("MagicCharmMinion ") =>
        Powers.MagicCharmMinionMovePower(getLetter(str.drop("MagicCharmMinion ".length)))
      case str if str.startsWith("RangedPush ") =>
        val List(letterStr, moraleCost, maxPushDistance) = str.drop("RangedPush ".length).split(" ").toList
        Powers.RangedPushMovePower(getLetter(letterStr), moraleCost.toInt, maxPushDistance.toInt)
      case str if str.startsWith("MagicPushFreeze ") =>
        val List(letterStr, maxPushDistance, freezeDuration) = str.drop("MagicPushFreeze ".length).split(" ").toList
        Powers.MagicPushFreeze(getLetter(letterStr), maxPushDistance.toInt, freezeDuration.toInt)
      case str if str.startsWith("MagicFreeze ") =>
        val List(letterStr, freezeDuration) = str.drop("MagicFreeze ".length).split(" ").toList
        Powers.MagicFreeze(getLetter(letterStr), freezeDuration.toInt)
      // Move Power Complete:
      case str if str.startsWith("KingCastling ") =>
        Powers.KingCastlingMovePowerComplete(str.drop("KingCastling ".length).split(" ").toList.map(getLetter))
      case str if str.startsWith("TeleportPiece ") =>
        Powers.TeleportPiecesMovePowerComplete(str.drop("TeleportPiece ".length).split(" ").toList.map(getLetter))
      // Positional Powers:
      case str if str.startsWith("OnMeleeDeathSpawnPieces ") =>
        val List(letterStr, pieceToCheck) = str.drop("OnMeleeDeathSpawnPieces ".length).split(" ").toList
        piecesToCheck = pieceToCheck :: piecesToCheck
        Powers.OnMeleeDeathSpawnPiecesPositionalPower(getLetter(letterStr), pieceToCheck)
      case str if str.startsWith("OnMeleeDeathKillAttackerPosition ") =>
        Powers.OnMeleeDeathKillAttackerPositionalPower(getLetter(str.drop("OnMeleeDeathKillAttackerPosition ".length)))
      // Augmented Move Powers:
      case "AugmentedTeleportGhast" =>
        Powers.AugmentedTeleportGhastMovePower
      case str =>
        throw new Exception("Unknown Power: " + str)
    }
  }

  def loadBoard(file: File): GameState = {
    val lines = Source.fromFile(file).getLines.toVector
    if (lines.size == 9 && lines(8).nonEmpty || lines.size > 9)
      throw new Exception(s"file $file has more than 8 lines!")

    var gameState = PlayGame.emptyGameState
    for (row <- lines.indices) {
      val line = lines(row).replaceAll("""\s+""", " ")
      val pieceNames = line.split(" ")
      for ((pieceName, column) <- pieceNames.zipWithIndex) {
        if (pieceName.length > 1) {
          val List(name, team) = pieceName.split("_").toList
          Try(getPieceData(name, PlayerTeam(team))) match {
            case Failure(_) =>
              piecesToCheck = name :: piecesToCheck
            case Success(pieceData) =>
              gameState = gameState.placePiece(Piece(pieceData, BoardPos(row, column)))
          }
        }
      }
    }

    gameState
  }
}
