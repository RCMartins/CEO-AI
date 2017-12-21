package ceo.play

import ceo.play.Moves._

sealed trait Powers

sealed trait MovePower extends Powers {
  def letterOfMove: Char

  def createMove(dx: Int, dy: Int): Moves
}

sealed trait MovePowerComplete extends Powers {
  def lettersOfMoves: List[Char]

  def createMove(dx: Int, dy: Int, char: Char, all: List[(Int, Int, Char)]): Moves
}

object Powers {

  case class PromoteTo(unitName: String) extends Powers

  case class LoseMoraleOnDeath(moraleAmount: Int) extends Powers

  case class GainMoraleOnKill(moraleAmount: Int) extends Powers

  case class DecayAfterTurn(turnStarts: Int, moralePerTurn: Int) extends Powers

  case class Immune(immuneList: List[String]) extends Powers

  case class DestroyedBy(destroyedBy: List[String]) extends Powers

  case object SuicideOnKill extends Powers

  case object GhostMovement extends Powers

  case class DummyNothingPower(letterOfMove: Char) extends MovePower {
    override def createMove(dx: Int, dy: Int): Moves = DummyMove
  }

  case class MagicDestroyMovePower(letterOfMove: Char) extends MovePower {
    override def createMove(dx: Int, dy: Int): Moves = MagicDestroy(dx, dy)
  }

  case class RangedPetrifyMovePower(letterOfMove: Char, durationTurns: Int) extends MovePower {
    override def createMove(dx: Int, dy: Int): Moves = RangedPetrify(dx, dy, durationTurns)
  }

  case class TaurusRushMovePower(letterOfMove: Char, maxDistance: Int) extends MovePower {
    override def createMove(dx: Int, dy: Int): Moves = TaurusRush(dx, dy, maxDistance)
  }

  case class TransformIntoAllyMovePower(letterOfMove: Char, moraleCost: Int, allyUnitName: String) extends MovePower {
    override def createMove(dx: Int, dy: Int): Moves = TransformEnemyIntoAllyUnit(dx, dy, moraleCost, allyUnitName)
  }

  case class JumpMinionMovePower(letterOfMove: Char) extends MovePower {
    override def createMove(dx: Int, dy: Int): Moves = JumpMinion(dx, dy)
  }

  case class KingCastlingMovePower(lettersOfMoves: List[Char]) extends MovePowerComplete {
    override def createMove(dx: Int, dy: Int, char: Char, all: List[(Int, Int, Char)]): Moves = {
      val pos = BoardPos(dx, dy)
      val dir = pos.normalize
      Castling(pos, pos - dir, pos - (dir * 2))
    }
  }

}
