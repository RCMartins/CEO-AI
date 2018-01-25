package ceo.play

import ceo.play.PlayerTeam._

sealed trait PlayerTeam {

  self =>

  val letter: Char

  val isWhite: Boolean = self == WhiteTop || self == WhiteBottom

  val isBlack: Boolean = !isWhite

  val enemy: PlayerTeam

  val color: PlayerColor = if (isWhite) PlayerColor.White else PlayerColor.Black

  val isBottom: Boolean = self == WhiteBottom || self == BlackBottom

  val isTop: Boolean = !isBottom

  def chooseWhiteBlack[A](whiteBranch: => A, blackBranch: => A): A = if (isWhite) whiteBranch else blackBranch
}

object PlayerTeam {

  case object WhiteTop extends PlayerTeam {
    val letter = 'W'
    val enemy: PlayerTeam = BlackTop
  }

  case object WhiteBottom extends PlayerTeam {
    val letter = 'W'
    val enemy: PlayerTeam = BlackBottom
  }

  case object BlackTop extends PlayerTeam {
    val letter = 'B'
    val enemy: PlayerTeam = WhiteTop
  }

  case object BlackBottom extends PlayerTeam {
    val letter = 'B'
    val enemy: PlayerTeam = WhiteBottom
  }

  def apply(str: String, whitePlayerInBottom: Boolean): PlayerTeam =
    if (str == "White") {
      if (whitePlayerInBottom) WhiteBottom else WhiteTop
    } else if (str == "Black") {
      if (whitePlayerInBottom) BlackTop else BlackBottom
    } else
      throw new Exception(s"Invalid team: $str")

}

sealed trait PlayerWinType

object PlayerWinType {

  case object PlayerWhite extends PlayerWinType

  case object PlayerBlack extends PlayerWinType

  case object Draw extends PlayerWinType

  case object NotFinished extends PlayerWinType

}

sealed trait PlayerColor

object PlayerColor {

  case object White extends PlayerColor

  case object Black extends PlayerColor

  def apply(str: String): PlayerColor =
    if (str == "White") White else if (str == "Black") Black else throw new Exception(s"Invalid team: $str")
}