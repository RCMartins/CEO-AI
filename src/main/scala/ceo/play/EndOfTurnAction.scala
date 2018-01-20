package ceo.play

trait EndOfTurnAction

object EndOfTurnAction {

  case class CreatePiece(newPiece: Piece) extends EndOfTurnAction

  case class PieceSwapWithEnemyKing(killerPiecePos: BoardPos) extends EndOfTurnAction

}