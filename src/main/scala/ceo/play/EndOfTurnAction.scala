package ceo.play

sealed trait EndOfTurnAction

object EndOfTurnAction {

  case class CreatePiece(newPiece: Piece) extends EndOfTurnAction

  case class PieceSwapWithEnemyKing(killerPiecePos: BoardPos) extends EndOfTurnAction

  case class MoveDoves(playerTeam: PlayerTeam) extends EndOfTurnAction

}
