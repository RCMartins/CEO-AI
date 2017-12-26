package ceo.play

import ceo.play.PlayerTeam.{Black, White}

sealed trait PlayerTeam {

  self =>

  val letter: Char

  val enemy: PlayerTeam = if (self == White) Black else White

  def chooseWhiteBlack[A](whiteBranch: A, blackBranch: A): A = if (self == White) whiteBranch else blackBranch
}

object PlayerTeam {

  case object White extends PlayerTeam {
    val letter = 'W'
  }

  case object Black extends PlayerTeam {
    val letter = 'B'
  }

  def apply(str: String): PlayerTeam =
    if (str == "White") White else if (str == "Black") Black else throw new Exception(s"Invalid team: $str")

}

sealed trait PlayerWinType

object PlayerWinType {

  case object PlayerWhite extends PlayerWinType

  case object PlayerBlack extends PlayerWinType

  case object Draw extends PlayerWinType

  case object NotFinished extends PlayerWinType

}