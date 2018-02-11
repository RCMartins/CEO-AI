package ceo.play

import ceo.play.Moves._

sealed trait Powers

sealed trait MovePower extends Powers {
  def letterOfMove: Char

  def createMove(dist: Distance): Moves
}

sealed trait PositionalPower extends Powers {
  def letterOfMove: Char

  def createPowers(distances: Map[Char, List[Distance]]): List[Powers]
}

sealed trait MovePowerComplete extends Powers {
  def lettersOfMoves: List[Char]

  def createMoves(distances: Map[Char, List[Distance]]): List[Moves]
}

sealed trait AugmentedMovePower extends Powers {
  def createMoves: List[Moves]
}

object Powers {

  case object OnAnyKillSuicides extends Powers

  case object GhostMovement extends Powers

  case object OnMeleeDeathKillAttacker extends Powers

  case object OnKillMercenary extends Powers

  case object CanOnlyActAfterPieceLost extends Powers

  case object OnMagicVanish extends Powers

  case object OnEnemyDeathMovesForward extends Powers

  case object OnMoveAdjacentHoplitesMove extends Powers

  case object CannotBeTargetedByMinions extends Powers

  case object WispReflect extends Powers

  case object OnChampionKillSwapEnemyKing extends Powers

  case object Dummy extends Powers

  case object OnKillGainHalfMorale extends Powers

  case object OnDeathHalfMoraleToKing extends Powers

  case class OnKillPromoteToKing(moraleBonus: Int) extends Powers

  case class OnMagicCastPromoteIfEnemy(pieceName: String) extends Powers

  case class OnAllyDeathPieceChangeMorale(moraleAmount: Int) extends Powers

  case class PromoteTo(pieceName: String) extends Powers

  case class OnAnyDeathPlayerChangeMorale(moraleAmount: Int) extends Powers

  case class OnKillPieceGainMorale(moraleAmount: Int) extends Powers

  case class OnKillPlayerChangeMorale(moraleAmount: Int) extends Powers

  case class OnDeathEnemyChangesMorale(moraleAmount: Int) extends Powers

  case class OnKillTransformInto(pieceName: String) extends Powers

  case class OnSpellCastPromoteTo(pieceName: String) extends Powers

  case class DecayAfterTurn(turnStarts: Int, moralePerTurn: Int) extends Powers

  case class ImmuneTo(immuneList: List[EffectType]) extends Powers

  case class DestroyedBy(destroyedBy: List[EffectType]) extends Powers

  case class OnMeleeDeathSpawnSlimes(distances: List[Distance], pieceName: String) extends Powers

  case class OnMeleeDeathKillAttackerFromPosition(distances: Set[Distance]) extends Powers

  case class BeginsGameEnchanted(enchantedDuration: Int) extends Powers

  case class TriggerGuardian(distances: Set[Distance]) extends Powers

  case class TriggerWrathOnAdjacentAllyDeath(turnsToLightUpLocation: Int) extends Powers

  case class TriggerFrostMephit(freezeDuration: Double) extends Powers

  case class OnKillVampireAbility(moraleTakenFromEnemy: Int, moraleToKing: Int) extends Powers

  case class OnMeleeDeathPoisonIfMoraleLess(maxMoraleToPoison: Int, turnsToDeath: Int) extends Powers

  case class OnKillDecayTo(moraleLostOnKill: Int, moraleLimit: Int, pieceName: String) extends Powers

  case class BlockAttacksFrom(positionsToBlockAttacks: Set[Distance]) extends Powers

  case class OnMagicCastDecayTo(decayAmount: Int, limitToDevolve: Int, pieceName: String) extends Powers

  case class GrowMoraleUntilTransform(moraleToPromote: Int, pieceName: String) extends Powers

  case class OnMagicCastDecayDeath(decayAmount: Int) extends Powers

  case class OnDeathEnchantAdjacentChampions(enchantDuration: Int) extends Powers

  case class OnDeathEnchantGlobalMinions(enchantDuration: Int) extends Powers

  case class TriggerInstantKill(distance: Distance) extends Powers

  case class OnMeleeDeathTriggerRevive(distance: Distance, moraleMinimum: Int) extends Powers

  case class HostageCaught(moraleAmount: Int) extends Powers

  case class OnDeathPhoenix(eggPieceName: String) extends Powers

  case class OnDeathTriggerFreezeMinions(freezeDistances: List[Distance], freezeDuration: Int) extends Powers

  case class MagicTriggerLust(lustDistances: List[Distance]) extends Powers

  case class DummyNothingPower(letterOfMove: Char) extends MovePower {
    override def createMove(dist: Distance): Moves = DummyMove
  }

  case class MagicDestroyMovePower(letterOfMove: Char) extends MovePower {
    override def createMove(dist: Distance): Moves = MagicDestroy(dist)
  }

  case class RangedPetrifyMovePower(letterOfMove: Char, durationTurns: Int) extends MovePower {
    override def createMove(dist: Distance): Moves = RangedPetrify(dist, durationTurns)
  }

  case class MagicPoisonMovePower(letterOfMove: Char, durationTurns: Int) extends MovePower {
    override def createMove(dist: Distance): Moves = MagicPoison(dist, durationTurns)
  }

  case class TaurusRushMovePower(letterOfMove: Char, maxDistance: Int) extends MovePower {
    override def createMove(dist: Distance): Moves = TaurusRush(dist, maxDistance)
  }

  case class TransformIntoAllyMovePower(letterOfMove: Char, moraleCost: Int, allyPieceName: String) extends MovePower {
    override def createMove(dist: Distance): Moves = MagicTransformEnemyIntoAllyPiece(dist, moraleCost, allyPieceName)
  }

  case class JumpMinionMovePower(letterOfMove: Char) extends MovePower {
    override def createMove(dist: Distance): Moves = JumpMinion(dist)
  }

  case class MagicCharmMinionMovePower(letterOfMove: Char) extends MovePower {
    override def createMove(dist: Distance): Moves = CharmMinion(dist)
  }

  case class RangedPushMovePower(letterOfMove: Char, moraleCost: Int, maxPushDistance: Int) extends MovePower {
    override def createMove(dist: Distance): Moves = RangedPush(dist, moraleCost, maxPushDistance)
  }

  case class MagicPushFreezeMovePower(letterOfMove: Char, maxPushDistance: Int, freezeDuration: Int) extends MovePower {
    override def createMove(dist: Distance): Moves = MagicPushFreezePiece(dist, maxPushDistance, freezeDuration)
  }

  case class MagicFreezeMovePower(letterOfMove: Char, freezeDuration: Int) extends MovePower {
    override def createMove(dist: Distance): Moves = MagicFreezePiece(dist, freezeDuration)
  }

  case class MagicLightningOnLocationMovePower(letterOfMove: Char, moraleCost: Int, turnsToLightUpLocation: Int) extends MovePower {
    override def createMove(dist: Distance): Moves = MagicLightning(dist, moraleCost, turnsToLightUpLocation)
  }

  case class UnstoppableTeleportTransformIntoMovePower(letterOfMove: Char, pieceName: String) extends MovePower {
    override def createMove(dist: Distance): Moves = UnstoppableTeleportTransformInto(dist, pieceName)
  }

  case class MagicStonePillarMovePower(letterOfMove: Char, moraleCost: Int, durationTurns: Int) extends MovePower {
    override def createMove(dist: Distance): Moves = MagicStonePillar(dist, moraleCost, durationTurns)
  }

  case class TeleportBeaconMovePower(letterOfMove: Char, augmentedRange: Int) extends MovePower {
    override def createMove(dist: Distance): Moves = MagicTeleportBeacon(dist, augmentedRange)
  }

  case class RangedSummonGeminiTwinMovePower(letterOfMove: Char, moraleCost: Int, pieceName: String) extends MovePower {
    override def createMove(dist: Distance): Moves = RangedSummonGeminiTwin(dist, moraleCost, pieceName)
  }

  case class MagicWeakEnchantMovePower(letterOfMove: Char, durationTurns: Int) extends MovePower {
    override def createMove(dist: Distance): Moves = MagicWeakEnchant(dist, durationTurns)
  }

  case class MagicEnvyCloneMovePower(letterOfMove: Char) extends MovePower {
    override def createMove(dist: Distance): Moves = MagicEnvyClone(dist)
  }

  case class MagicMeteorMovePower(letterOfMove: Char, moraleCost: Int, turnsToMeteor: Int) extends MovePower {
    override def createMove(dist: Distance): Moves = MagicMeteor(dist, moraleCost, turnsToMeteor)
  }

  case class MagicPushMovePower(letterOfMove: Char, moraleCost: Int, pushDistance: Int) extends MovePower {
    override def createMove(dist: Distance): Moves = MagicPush(dist, moraleCost, pushDistance)
  }

  case class RangedPushSpawnMovePower(letterOfMove: Char, moraleCost: Int, maxPushDistance: Int, pieceName: String) extends MovePower {
    override def createMove(dist: Distance): Moves = RangedPushSpawn(dist, moraleCost, maxPushDistance, pieceName)
  }

  case class MagicSummonPieceMovePower(letterOfMove: Char, moraleCost: Int, pieceName: String) extends MovePower {
    override def createMove(dist: Distance): Moves = MagicSummonPiece(dist, moraleCost, pieceName)
  }

  case class TemperanceAttackUnblockableMovePower(letterOfMove: Char) extends MovePower {
    override def createMove(dist: Distance): Moves = TemperanceAttackUnblockable(dist)
  }

  case class TemperanceAttackOrMoveMovePower(letterOfMove: Char) extends MovePower {
    override def createMove(dist: Distance): Moves = TemperanceAttackOrMove(dist)
  }

  case class RangedCompelMovePower(letterOfMove: Char, turnsCompeled: Int) extends MovePower {
    override def createMove(dist: Distance): Moves = RangedCompel(dist, turnsCompeled)
  }

  case class MagicDestroySelfButterflyMovePower(letterOfMove: Char, turnsDelay: Int, turnsEnchanted: Int) extends MovePower {
    override def createMove(dist: Distance): Moves = MagicDestroySelfButterfly(dist, turnsDelay, turnsEnchanted)
  }

  case class TeleportManyToOneMovePowerComplete(lettersOfMoves: List[Char]) extends MovePowerComplete {
    override def createMoves(distances: Map[Char, List[Distance]]): List[Moves] = {
      val List(c1, c2) = lettersOfMoves
      val distances1 = distances(c1)
      val distances2 = distances(c2)
      List(TeleportManyToOne(distances1, distances2))
    }
  }

  case class TeleportOneToManyMovePowerComplete(lettersOfMoves: List[Char]) extends MovePowerComplete {
    override def createMoves(distances: Map[Char, List[Distance]]): List[Moves] = {
      val List(c1, c2) = lettersOfMoves
      val distances1 = distances(c1)
      val distances2 = distances(c2)
      List(TeleportOneToMany(distances1, distances2))
    }
  }

  case class TeleportKingToLocationMovePowerComplete(lettersOfMoves: List[Char]) extends MovePowerComplete {
    override def createMoves(distances: Map[Char, List[Distance]]): List[Moves] = {
      List(TeleportKingToLocation(distances.values.flatten.toList))
    }
  }

  case class MagicMoveTargetTowardsMovePowerComplete(letterFrom: Char, letterTowards: Char, moraleCost: Int) extends MovePowerComplete {
    val lettersOfMoves: List[Char] = List(letterFrom, letterTowards)

    override def createMoves(distances: Map[Char, List[Distance]]): List[Moves] = {
      val from = distances(letterFrom).head
      distances(letterTowards).map { distanceTowards =>
        val maxDistance = distanceTowards - from
        MagicPushTowards(distanceTowards, moraleCost, maxDistance)
      }
    }
  }

  case class PatienceCannotAttackBeforeTurnMovePowerComplete(lettersOfMoves: List[Char], untilTurn: Int) extends MovePowerComplete {
    override def createMoves(distances: Map[Char, List[Distance]]): List[Moves] = {
      val List(moveOrAttack, attack) = lettersOfMoves.map(distances)
      List(PatienceCannotAttackBeforeTurn(moveOrAttack, attack, untilTurn))
    }
  }

  case class AugmentedTeleportRoyalGuardPowerComplete(lettersOfMoves: List[Char]) extends MovePowerComplete {
    override def createMoves(distances: Map[Char, List[Distance]]): List[Moves] = {
      val moveOrAttackList = distances.getOrElse('1', List.empty)
      val moveList = distances.getOrElse('2', List.empty)
      val teleportList = distances.getOrElse('3', List.empty)
      List(TeleportToRoyalPieces(moveOrAttackList, moveList, teleportList))
    }
  }

  case class ChastityMovePowerComplete(lettersOfMoves: List[Char]) extends MovePowerComplete {
    override def createMoves(distances: Map[Char, List[Distance]]): List[Moves] = {
      val moveOrAttackList = distances.getOrElse('1', List.empty)
      val attackList = distances.getOrElse('2', List.empty)
      val attackUnblockableList = distances.getOrElse('3', List.empty)
      val swapList = distances.getOrElse('4', List.empty)
      List(ChastityMoves(moveOrAttackList, attackList, attackUnblockableList, swapList))
    }
  }

  case class OnMeleeDeathSpawnSlimesPositionalPower(letterOfMove: Char, allyPieceName: String) extends PositionalPower {
    def createPowers(distances: Map[Char, List[Distance]]): List[Powers] =
      List(OnMeleeDeathSpawnSlimes(distances.values.flatten.toList, allyPieceName))
  }

  case class OnMeleeDeathKillAttackerPositionalPower(letterOfMove: Char) extends PositionalPower {
    def createPowers(distances: Map[Char, List[Distance]]): List[Powers] =
      List(OnMeleeDeathKillAttackerFromPosition(distances.values.flatten.toSet))
  }

  case class TriggerGuardianPositionalPower(letterOfMove: Char) extends PositionalPower {
    def createPowers(distances: Map[Char, List[Distance]]): List[Powers] =
      List(TriggerGuardian(distances.values.flatten.toSet))
  }

  case class TriggerInstantKillPositionalPower(letterOfMove: Char) extends PositionalPower {
    def createPowers(distances: Map[Char, List[Distance]]): List[Powers] =
      List(TriggerInstantKill(distances.values.flatten.head))
  }

  case class OnMeleeDeathTriggerRevivePositionalPower(letterOfMove: Char, moraleMinimum: Int) extends PositionalPower {
    def createPowers(distances: Map[Char, List[Distance]]): List[Powers] =
      List(OnMeleeDeathTriggerRevive(distances.values.flatten.head, moraleMinimum))
  }

  case class OnDeathTriggerFreezeMinionsPositionalPower(letterOfMove: Char, freezeDuration: Int) extends PositionalPower {
    def createPowers(distances: Map[Char, List[Distance]]): List[Powers] =
      List(OnDeathTriggerFreezeMinions(distances.values.flatten.toList, freezeDuration))
  }

  case class MagicTriggerLustPositionalPower(letterOfMove: Char) extends PositionalPower {
    def createPowers(distances: Map[Char, List[Distance]]): List[Powers] =
      List(MagicTriggerLust(distances.values.flatten.toList))
  }

  case object AugmentedTeleportGhastMovePower extends AugmentedMovePower {
    override def createMoves: List[Moves] = List(
      TeleportToFallenAllyPosition
    )
  }

  case class MagicFreezeStrikeGlobalEnemyChampion(freezeDuration: Int) extends AugmentedMovePower {
    override def createMoves: List[Moves] = List(
      MagicFreezeStrikeOnEnemyChampions(freezeDuration)
    )
  }

  case object KingCastlingPower extends AugmentedMovePower {
    override def createMoves: List[Moves] = {
      List(KingCastling)
    }
  }

}
