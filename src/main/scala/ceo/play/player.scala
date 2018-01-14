package ceo.play

import ceo.play.PlayerTeam.White

case class Player(
  team: PlayerTeam,
  morale: Int,
  pieces: List[Piece] = List.empty,
  piecesAffected: List[Piece] = List.empty,
  numberOfPieces: Int,
  hasKing: Boolean = false,
  extraData: PlayerExtraData
) {

  override def toString: String = s"Player$team($morale)"

  def allPieces: List[Piece] = pieces ++ piecesAffected

  def changeMorale(diff: Int): Player = copy(morale = morale + diff)

  def removePiece(piece: Piece): Player = {
    if (piece.data.isKing)
      copy(pieces = pieces.filterNot(_ eq piece),
        piecesAffected = piecesAffected.filterNot(_ eq piece), numberOfPieces = numberOfPieces - 1, hasKing = false)
    else
      copy(pieces = pieces.filterNot(_ eq piece),
        piecesAffected = piecesAffected.filterNot(_ eq piece), numberOfPieces = numberOfPieces - 1)
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

  // TODO: not used yet
  def killAt(boardPos: BoardPos): Player =
    copy(extraData = extraData.copy(fallenPiecesPositions = extraData.fallenPiecesPositions + boardPos))

  def updateGuardedPositions(toRemove: Option[Piece], toAdd: Option[Piece]): Player = {
    val updatedGuardedPositions =
      (toRemove, toAdd) match {
        case (Some(piece1), Some(piece2)) =>
          extraData.guardedPositions -- piece1.data.guardedPositions.map(piece1.pos + _) ++
            piece2.data.guardedPositions.map(dist => (piece2.pos + dist) -> piece2)
        case (Some(piece), None) =>
          extraData.guardedPositions -- piece.data.guardedPositions.map(piece.pos + _)
        case (None, Some(piece)) =>
          extraData.guardedPositions ++ piece.data.guardedPositions.map(dist => (piece.pos + dist) -> piece)
        case (None, None) =>
          extraData.guardedPositions
      }
    copy(extraData = extraData.copy(guardedPositions = updatedGuardedPositions))
  }

  def inBaseRow(pos: BoardPos): Boolean = pos.row == (if (team == White) 7 else 0)

  def directionForward: Distance = if (team == White) Distance(-1, 0) else Distance(1, 0)
}

case class PlayerExtraData(
  fallenPiecesPositions: Set[BoardPos],
  guardedPositions: Map[BoardPos, Piece]
)

object PlayerExtraData {
  val empty: PlayerExtraData = PlayerExtraData(Set.empty, Map.empty)
}
