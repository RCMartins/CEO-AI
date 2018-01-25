package ceo.play

sealed trait BoardEffect

object BoardEffect {

  case class Lightning(boardPos: BoardPos, turnOfLightning: Double) extends BoardEffect

  case class Meteor(boardPos: BoardPos, turnOfMeteor: Double, pushDistance: Int) extends BoardEffect

}
