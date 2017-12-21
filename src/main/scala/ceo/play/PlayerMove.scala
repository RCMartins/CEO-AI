package ceo.play

/**
  * Created on 17-Dec-17.
  */
sealed trait PlayerMove {

  def betterHumanString: String

}

object PlayerMove {

  case class MultiMove(move1: PlayerMove, move2: PlayerMove) extends PlayerMove {
    def betterHumanString: String = s"$move1 AND $move2"
  }

  case class Move(piece: Piece, to: BoardPos) extends PlayerMove {
    def betterHumanString: String = s"$piece moves to $to   ${to - piece.pos}"
  }

  case class Attack(piece: Piece, pieceToKill: Piece) extends PlayerMove {
    def betterHumanString: String = s"$piece attacks $pieceToKill   ${pieceToKill.pos - piece.pos}"
  }

  case class Swap(piece: Piece, pieceToSwap: Piece) extends PlayerMove {
    def betterHumanString: String = s"$piece swaps with $pieceToSwap   ${pieceToSwap.pos - piece.pos}"
  }

  case class RangedDestroy(piece: Piece, pieceToDestroy: Piece) extends PlayerMove {
    def betterHumanString: String = s"$piece ranged-destroys $pieceToDestroy   ${pieceToDestroy.pos - piece.pos}"
  }

  case class MagicDestroy(piece: Piece, pieceToDestroy: Piece) extends PlayerMove {
    def betterHumanString: String = s"$piece magic-destroys $pieceToDestroy   ${pieceToDestroy.pos - piece.pos}"
  }

  case class RangedPetrify(piece: Piece, pieceToPetrify: Piece, turnsPetrified: Int) extends PlayerMove {
    def betterHumanString: String = s"$piece ranged-petrifies $pieceToPetrify   ${pieceToPetrify.pos - piece.pos}"
  }

  case class TransformEnemyIntoAllyUnit(
    piece: Piece,
    pieceToTransform: Piece,
    moraleCost: Int,
    allyPieceData: PieceData
  ) extends PlayerMove {
    def betterHumanString: String =
      s"$piece transforms[$moraleCost cost] $pieceToTransform into ${allyPieceData.name}   ${pieceToTransform.pos - piece.pos}"
  }

  case class TaurusRush(piece: Piece, pieceToRush: Piece, maxDistance: Int) extends PlayerMove {
    def betterHumanString: String = s"$piece RushEnemy $pieceToRush   ${pieceToRush.pos - piece.pos}"
  }

  case class DummyMove(piece: Piece) extends PlayerMove {
    def betterHumanString: String = s"$piece does nothing special"
  }

}