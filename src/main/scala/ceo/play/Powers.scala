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
  def createMove: List[Moves]
}

sealed trait InitialStatusEffect {
  def getInitialStatusEffects: List[EffectStatus]
}

object Powers {

  case object OnAnyKillSuicides extends Powers

  case object GhostMovement extends Powers

  case object OnMeleeDeathKillAttacker extends Powers

  case object OnKillMercenary extends Powers

  case object CanOnlyActAfterPieceLost extends Powers

  case class PromoteTo(pieceName: String) extends Powers

  case class OnAnyDeathPlayerChangeMorale(moraleAmount: Int) extends Powers

  case class PieceChangeMoraleOnKill(moraleAmount: Int) extends Powers

  case class PlayerChangeMoraleOnKill(moraleAmount: Int) extends Powers

  case class OnKillTransformInto(pieceName: String) extends Powers

  case class PromoteOnSpellCastTo(pieceName: String) extends Powers

  case class DecayAfterTurn(turnStarts: Int, moralePerTurn: Int) extends Powers

  case class ImmuneTo(immuneList: List[EffectType]) extends Powers

  // TODO do this validation
  case class DestroyedBy(destroyedBy: List[EffectType]) extends Powers

  case class OnMeleeDeathSpawnPieces(distances: List[Distance], pieceName: String) extends Powers

  case class OnMeleeDeathKillAttackerFromPosition(distances: Set[Distance]) extends Powers

  case class TriggerGuardian(distances: Set[Distance]) extends Powers

  case class TriggerWrathOnAdjacentAllyDeath(turnsToLightUpLocation: Int) extends Powers

  case class TriggerFrostMephit(freezeDuration: Int) extends Powers

  case class OnKillVampireAbility(moraleTakenFromEnemy: Int, moraleToKing: Int) extends Powers

  case class BlockAttacksFrom(positionsToBlockAttacks: Set[Distance]) extends Powers

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
    override def createMove(dist: Distance): Moves = TransformEnemyIntoAllyPiece(dist, moraleCost, allyPieceName)
  }

  case class JumpMinionMovePower(letterOfMove: Char) extends MovePower {
    override def createMove(dist: Distance): Moves = JumpMinion(dist)
  }

  case class MagicCharmMinionMovePower(letterOfMove: Char) extends MovePower {
    override def createMove(dist: Distance): Moves = CharmMinion(dist)
  }

  case class RangedPushMovePower(letterOfMove: Char, moraleCost: Int, maxPushDistance: Int) extends MovePower {
    override def createMove(dist: Distance): Moves = PushPiece(dist, moraleCost, maxPushDistance)
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

  case class KingCastlingMovePowerComplete(lettersOfMoves: List[Char]) extends MovePowerComplete {
    override def createMoves(distances: Map[Char, List[Distance]]): List[Moves] = {
      distances.values.flatten.toList.map {
        dist =>
          val dir = dist.toUnitVector
          Castling(dist, dist - dir, dist - (dir * 2))
      }
    }
  }

  case class TeleportPiecesMovePowerComplete(lettersOfMoves: List[Char]) extends MovePowerComplete {
    override def createMoves(distances: Map[Char, List[Distance]]): List[Moves] = {
      lettersOfMoves.grouped(2).toList.flatMap {
        case List(c1, c2) =>
          for {
            dist1 <- distances(c1)
            dist2 <- distances(c2)
          } yield TeleportPiece(dist1, dist2)
      }
    }
  }

  case class TeleportKingToLocationMovePowerComplete(lettersOfMoves: List[Char]) extends MovePowerComplete {
    override def createMoves(distances: Map[Char, List[Distance]]): List[Moves] = {
      List(TeleportKingToLocation(distances.values.flatten.toList))
    }
  }

  case class OnMeleeDeathSpawnPiecesPositionalPower(letterOfMove: Char, allyPieceName: String) extends PositionalPower {
    def createPowers(distances: Map[Char, List[Distance]]): List[Powers] =
      List(OnMeleeDeathSpawnPieces(distances.values.flatten.toList, allyPieceName))
  }

  case class OnMeleeDeathKillAttackerPositionalPower(letterOfMove: Char) extends PositionalPower {
    def createPowers(distances: Map[Char, List[Distance]]): List[Powers] =
      List(OnMeleeDeathKillAttackerFromPosition(distances.values.flatten.toSet))
  }

  case class TriggerGuardianPositionalPower(letterOfMove: Char) extends PositionalPower {
    def createPowers(distances: Map[Char, List[Distance]]): List[Powers] =
      List(TriggerGuardian(distances.values.flatten.toSet))
  }

  case class TriggerFrostMephitPositionalPower(letterOfMove: Char, freezeDuration: Int) extends PositionalPower {
    def createPowers(distances: Map[Char, List[Distance]]): List[Powers] =
      List(TriggerFrostMephit(freezeDuration))
  }

  case object AugmentedTeleportGhastMovePower extends AugmentedMovePower {
    override def createMove: List[Moves] = List(
      TeleportToFallenAllyPosition
    )
  }

  case object AugmentedTeleportRoyalGuard extends AugmentedMovePower {
    override def createMove: List[Moves] = List(
      TeleportToRoyalPieces
    )
  }

}
