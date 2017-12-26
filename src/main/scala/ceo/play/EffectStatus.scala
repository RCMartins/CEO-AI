package ceo.play

sealed trait EffectStatusType {
  def name: String
}

object EffectStatusType {

  case object Petrify extends EffectStatusType {
    val name = "Petrify"
  }

  case object Poison extends EffectStatusType {
    val name = "Poison"
  }

  case object Freeze extends EffectStatusType {
    val name = "Freeze"
  }

  val all: List[EffectStatusType] = List(Petrify, Poison, Freeze)

  def apply(name: String): EffectStatusType = all.find(_.name == name) match {
    case Some(effectStatusType) => effectStatusType
    case None => throw new Exception(s"Unknown status effect: $name")
  }
}

sealed trait EffectStatus {
  def effectType: EffectStatusType
}

object EffectStatus {

  case class Petrified(untilTurn: Double) extends EffectStatus {
    override val effectType: EffectStatusType = EffectStatusType.Petrify
  }

  case class Poison(turnOfDeath: Double) extends EffectStatus {
    override val effectType: EffectStatusType = EffectStatusType.Poison
  }

  case class Frozen(untilTurn: Double) extends EffectStatus {
    override val effectType: EffectStatusType = EffectStatusType.Freeze
  }

}

