package ceo.play

import ceo.play.Moves._

sealed trait Powers

sealed trait MovePower extends Powers {
  def letterOfMove: Char

  def createMove(dist: Distance): Moves
}

sealed trait MovePowerComplete extends Powers {
  def lettersOfMoves: List[Char]

  def createMove(dist: Distance, char: Char, all: List[(Distance, Char)]): Moves
}

sealed trait InitialStatusEffect {
  def getInitialStatusEffects: List[EffectStatus]
}

object Powers {

  case class PromoteTo(unitName: String) extends Powers

  case class LoseMoraleOnDeath(moraleAmount: Int) extends Powers

  case class GainMoraleOnKill(moraleAmount: Int) extends Powers

  case class OnKillTransform(unitName: String) extends Powers

  case class DecayAfterTurn(turnStarts: Int, moralePerTurn: Int) extends Powers

  case class Immune(immuneList: List[String]) extends Powers

  case class DestroyedBy(destroyedBy: List[String]) extends Powers

  case object SuicideOnKill extends Powers

  case object GhostMovement extends Powers

  case class DummyNothingPower(letterOfMove: Char) extends MovePower {
    override def createMove(dist: Distance): Moves = DummyMove
  }

  case class MagicDestroyMovePower(letterOfMove: Char) extends MovePower {
    override def createMove(dist: Distance): Moves = MagicDestroy(dist)
  }

  case class RangedPetrifyMovePower(letterOfMove: Char, durationTurns: Int) extends MovePower {
    override def createMove(dist: Distance): Moves = RangedPetrify(dist, durationTurns)
  }

  case class TaurusRushMovePower(letterOfMove: Char, maxDistance: Int) extends MovePower {
    override def createMove(dist: Distance): Moves = TaurusRush(dist, maxDistance)
  }

  case class TransformIntoAllyMovePower(letterOfMove: Char, moraleCost: Int, allyUnitName: String) extends MovePower {
    override def createMove(dist: Distance): Moves = TransformEnemyIntoAllyUnit(dist, moraleCost, allyUnitName)
  }

  case class JumpMinionMovePower(letterOfMove: Char) extends MovePower {
    override def createMove(dist: Distance): Moves = JumpMinion(dist)
  }

  case class KingCastlingMovePower(lettersOfMoves: List[Char]) extends MovePowerComplete {
    override def createMove(dist: Distance, char: Char, all: List[(Distance, Char)]): Moves = {
      val dir = dist.toUnitVector
      Castling(dist, dist - dir, dist - (dir * 2))
    }
  }

}
