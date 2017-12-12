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
  val color: PlayerColor
  val morale: Int
  val pieces: List[Piece]

  def enemyColor: PlayerColor = if (color == White) Black else White
}

object Player {

  case class PlayerWhite(morale: Int, pieces: List[Piece] = List.empty) extends Player {
    val color: PlayerColor = PlayerColor.White

    def increaseMorale(amount: Int): PlayerWhite = copy(morale = morale + amount)

    def decreaseMorale(amount: Int): PlayerWhite = copy(morale = morale - amount)
  }

  case class PlayerBlack(morale: Int, pieces: List[Piece] = List.empty) extends Player {
    val color: PlayerColor = PlayerColor.Black

    def increaseMorale(amount: Int): PlayerBlack = copy(morale = morale + amount)

    def decreaseMorale(amount: Int): PlayerBlack = copy(morale = morale - amount)
  }

}

sealed trait PlayerMove

object PlayerMove {

  case class Move(piece: Piece, to: BoardPos) extends PlayerMove

  case class Attack(piece: Piece, pieceToKill: Piece) extends PlayerMove

  case class RangedDestroy(piece: Piece, pieceToDestroy: Piece) extends PlayerMove

}
