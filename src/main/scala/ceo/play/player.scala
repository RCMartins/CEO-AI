package ceo.play

import ceo.play.PlayerTeam.White

case class Player(
  team: PlayerTeam,
  morale: Int,
  pieces: List[Piece] = List.empty,
  piecesAffected: List[Piece] = List.empty,
  numberOfPieces: Int,
  kingMode: Int,
  extraData: PlayerExtraData
) {

  override def toString: String = s"Player$team($morale)"

  def hasKing: Boolean = kingMode > 0

  def allPieces: List[Piece] = pieces ++ piecesAffected

  def changeMorale(diff: Int): Player = copy(morale = morale + diff)

  def removePiece(piece: Piece): Player = {
    def removeFirst(list: List[Piece]): List[Piece] = list match {
      case Nil => Nil
      case x :: xs => if (x eq piece) xs else x :: removeFirst(xs)
    }

    if (piece.data.isKing)
      copy(pieces = removeFirst(pieces),
        piecesAffected = removeFirst(piecesAffected), numberOfPieces = numberOfPieces - 1, kingMode = kingMode - 1)
    else
      copy(pieces = removeFirst(pieces),
        piecesAffected = removeFirst(piecesAffected), numberOfPieces = numberOfPieces - 1)
  }

  def placePiece(piece: Piece): Player = {
    val isAffected = piece.effectStatus.nonEmpty
    if (piece.data.isKing) {
      if (isAffected)
        copy(piecesAffected = piece :: piecesAffected, numberOfPieces = numberOfPieces + 1, kingMode = if (kingMode <= 0) 1 else kingMode + 1)
      else
        copy(pieces = piece :: pieces, numberOfPieces = numberOfPieces + 1, kingMode = if (kingMode <= 0) 1 else kingMode + 1)
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
