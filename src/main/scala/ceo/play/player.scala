package ceo.play

import ceo.play.PlayerColor.{Black, White}

sealed trait PlayerColor {

  val letter: Char

  def enemy: PlayerColor = if (this == White) Black else White

  def chooseWhiteBlack[A](whiteBranch: A, blackBranch: A): A = if (this == White) whiteBranch else blackBranch
}

object PlayerColor {

  case object White extends PlayerColor {
    val letter = 'W'
  }

  case object Black extends PlayerColor {
    val letter = 'B'
  }

  def apply(str: String): PlayerColor = if (str == "White") White else Black

}

trait Player {
  val team: PlayerColor
  val morale: Int
  val pieces: List[Piece]

  def enemyColor: PlayerColor = if (team == White) Black else White
}

object Player {

  case class PlayerWhite(morale: Int, pieces: List[Piece] = List.empty) extends Player {
    val team: PlayerColor = PlayerColor.White

    def increaseMorale(amount: Int): PlayerWhite = copy(morale = morale + amount)

    def decreaseMorale(amount: Int): PlayerWhite = copy(morale = morale - amount)

    def removePiece(piece: Piece): PlayerWhite = copy(pieces = pieces.filterNot(_ == piece))

    def placePiece(piece: Piece): PlayerWhite = copy(pieces = piece :: pieces)
  }

  case class PlayerBlack(morale: Int, pieces: List[Piece] = List.empty) extends Player {
    val team: PlayerColor = PlayerColor.Black

    def increaseMorale(amount: Int): PlayerBlack = copy(morale = morale + amount)

    def decreaseMorale(amount: Int): PlayerBlack = copy(morale = morale - amount)

    def removePiece(piece: Piece): PlayerBlack = copy(pieces = pieces.filterNot(_ == piece))

    def placePiece(piece: Piece): PlayerBlack = copy(pieces = piece :: pieces)
  }

}

sealed trait PlayerMove

object PlayerMove {

  case class Move(piece: Piece, to: BoardPos) extends PlayerMove

  case class Attack(piece: Piece, pieceToKill: Piece) extends PlayerMove

  case class RangedDestroy(piece: Piece, pieceToDestroy: Piece) extends PlayerMove

}
