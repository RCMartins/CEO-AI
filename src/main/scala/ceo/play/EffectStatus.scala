package ceo.play

sealed trait EffectStatus

object EffectStatus {

  case class Petrified(untilTurn: Double) extends EffectStatus

}

