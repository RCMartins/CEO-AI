package ceo.play

import ceo.play.Moves._

sealed trait Powers

sealed trait MovePower extends Powers {
  def letterOfMove: Char

  def createMove(dist: Distance): Moves
}

sealed trait PositionalPower extends Powers {
  def letterOfMove: Char

  def createPower(positionalPowerPos: List[(Distance, Char)]): List[Powers]
}

sealed trait MovePowerComplete extends Powers {
  def lettersOfMoves: List[Char]

  def createMove(dist: Distance, char: Char, all: List[(Distance, Char)]): Moves
}

sealed trait InitialStatusEffect {
  def getInitialStatusEffects: List[EffectStatus]
}

object Powers {

  case object OnKillSuicide extends Powers

  case object GhostMovement extends Powers

  case object OnMeleeDeathKillAttacker extends Powers

  case object OnKillMercenary extends Powers

  case class PromoteTo(pieceName: String) extends Powers

  case class LoseMoraleOnDeath(moraleAmount: Int) extends Powers

  case class GainMoraleOnKill(moraleAmount: Int) extends Powers

  case class OnKillTransformInto(pieceName: String) extends Powers

  case class DecayAfterTurn(turnStarts: Int, moralePerTurn: Int) extends Powers

  // TODO do this validation
  case class ImmuneTo(immuneList: List[EffectStatusType]) extends Powers

  case class DestroyedBy(destroyedBy: List[EffectStatusType]) extends Powers

  case class OnMeleeDeathSpawnPieces(distances: List[Distance], pieceName: String) extends Powers

  case class OnMeleeDeathKillAttackerFromPosition(distances: List[Distance]) extends Powers

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

  case class KingCastlingMovePower(lettersOfMoves: List[Char]) extends MovePowerComplete {
    override def createMove(dist: Distance, char: Char, all: List[(Distance, Char)]): Moves = {
      val dir = dist.toUnitVector
      Castling(dist, dist - dir, dist - (dir * 2))
    }
  }

  case class OnMeleeDeathSpawnPiecesPositionalPower(letterOfMove: Char, allyPieceName: String) extends PositionalPower {
    def createPower(positions: List[(Distance, Char)]): List[Powers] =
      List(OnMeleeDeathSpawnPieces(positions.map(_._1), allyPieceName))
  }

  case class OnMeleeDeathKillAttackerPositionalPower(letterOfMove: Char) extends PositionalPower {
    def createPower(positions: List[(Distance, Char)]): List[Powers] =
      List(OnMeleeDeathKillAttackerFromPosition(positions.map(_._1)))
  }

}
