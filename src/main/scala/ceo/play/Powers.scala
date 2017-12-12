package ceo.play

sealed trait Powers

object Powers {

  case class PromoteTo(unitName: String) extends Powers

  case class DeathMoraleLost(moraleAmount: Int) extends Powers

  case class Immune(immuneList: List[String]) extends Powers

  case class DestroyedBy(destroyedBy: List[String]) extends Powers

  case object KingCastling extends Powers

  case object SuicideOnKill extends Powers

}
