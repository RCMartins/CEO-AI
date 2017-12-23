package ceo.play

import ceo.play.PlayerTeam.{Black, White}

sealed trait PlayerTeam {

  val letter: Char

  def enemy: PlayerTeam = if (this == White) Black else White

  def chooseWhiteBlack[A](whiteBranch: A, blackBranch: A): A = if (this == White) whiteBranch else blackBranch
}

object PlayerTeam {

  case object White extends PlayerTeam {
    val letter = 'W'
  }

  case object Black extends PlayerTeam {
    val letter = 'B'
  }

  def apply(str: String): PlayerTeam = if (str == "White") White else if (str == "Black") Black else ???

}

case class Player(
  team: PlayerTeam,
  morale: Int,
  pieces: List[Piece] = List.empty,
  numberOfPieces: Int,
  hasKing: Boolean = false
) {

  override def toString: String = s"Player$team($morale)"

  def changeMorale(diff: Int): Player = copy(morale = morale + diff)

  def removePiece(piece: Piece): Player = {
    if (piece.data.isKing)
      copy(pieces = pieces.filterNot(_ == piece), numberOfPieces = numberOfPieces - 1, hasKing = false)
    else
      copy(pieces = pieces.filterNot(_ == piece), numberOfPieces = numberOfPieces - 1)
  }

  def placePiece(piece: Piece): Player = {
    if (piece.data.isKing)
      copy(pieces = piece :: pieces, numberOfPieces = numberOfPieces + 1, hasKing = true)
    else
      copy(pieces = piece :: pieces, numberOfPieces = numberOfPieces + 1)
  }

  def inBaseRow(pos: BoardPos): Boolean = pos.row == (if (team == White) 7 else 0)
}
