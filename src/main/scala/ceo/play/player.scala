package ceo.play

import ceo.play.PlayerTeam.White

case class Player(
  team: PlayerTeam,
  morale: Int,
  pieces: List[Piece] = List.empty,
  piecesAffected: List[Piece] = List.empty,
  numberOfPieces: Int,
  hasKing: Boolean = false
) {

  override def toString: String = s"Player$team($morale)"

  def changeMorale(diff: Int): Player = copy(morale = morale + diff)

  def removePiece(piece: Piece): Player = {
    if (piece.data.isKing)
      copy(pieces = pieces.filterNot(_ eq piece), piecesAffected = piecesAffected.filterNot(_ eq piece), numberOfPieces = numberOfPieces - 1, hasKing = false)
    else
      copy(pieces = pieces.filterNot(_ eq piece), piecesAffected = piecesAffected.filterNot(_ eq piece), numberOfPieces = numberOfPieces - 1)
  }

  def placePiece(piece: Piece): Player = {
    val isAffected = piece.effectStatus.nonEmpty
    if (piece.data.isKing) {
      if (isAffected)
        copy(piecesAffected = piece :: piecesAffected, numberOfPieces = numberOfPieces + 1, hasKing = true)
      else
        copy(pieces = piece :: pieces, numberOfPieces = numberOfPieces + 1, hasKing = true)
    } else {
      if (isAffected)
        copy(piecesAffected = piece :: piecesAffected, numberOfPieces = numberOfPieces + 1)
      else
        copy(pieces = piece :: pieces, numberOfPieces = numberOfPieces + 1)
    }
  }

  def inBaseRow(pos: BoardPos): Boolean = pos.row == (if (team == White) 7 else 0)
}
