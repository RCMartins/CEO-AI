package ceo.play

trait BoardEffect

object BoardEffect {

  case class Lightning(boardPos: BoardPos, turnOfLightning: Double) extends BoardEffect

}
