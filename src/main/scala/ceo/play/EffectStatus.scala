package ceo.play

sealed trait EffectType {
  def name: String
}

object EffectType {

  case object Petrify extends EffectType {
    val name = "Petrify"
  }

  case object Poison extends EffectType {
    val name = "Poison"
  }

  case object Freeze extends EffectType {
    val name = "Freeze"
  }

  case object Enchanted extends EffectType {
    val name = "Enchanted"
  }

  case object WeakEnchanted extends EffectType {
    val name = "WeakEnchanted"
  }

  case object Displacement extends EffectType {
    val name = "Displacement"
  }

  case object Magic extends EffectType {
    val name = "Magic"
  }

  case object Ranged extends EffectType {
    val name = "Ranged"
  }

  case object Trigger extends EffectType {
    val name = "Trigger"
  }

  case object Compel extends EffectType {
    val name = "Compel"
  }

  case object BlockAttacks extends EffectType {
    val name = "BlockAttacks"
  }

  case object InstantKillPositional extends EffectType {
    val name = "InstantKillPositional"
  }

  case object PieceGrowOnPlayerTurn extends EffectType {
    val name = "PieceGrowOnPlayerTurn"
  }

  case object PieceGrowOnOpponentTurn extends EffectType {
    val name = "PieceGrowOnOpponentTurn"
  }
  }

  case object DecayAfterTurn extends EffectType {
    val name = "DecayAfterTurn"
  }

  val allNormalEffects: List[EffectType] = List(Petrify, Poison, Freeze, Displacement, Magic, Ranged, Trigger, Compel)

  def apply(name: String): EffectType = allNormalEffects.find(_.name == name) match {
    case Some(effectStatusType) => effectStatusType
    case None => throw new Exception(s"Unknown status effect: $name")
  }
}

sealed trait EffectStatus {
  def effectType: EffectType

  def getReplayInfo: String = ""
}

object EffectStatus {

  case class Petrified(untilTurn: Double) extends EffectStatus {
    override val effectType: EffectType = EffectType.Petrify

    override def getReplayInfo = s"Petrified($untilTurn)"
  }

  case class Poison(turnOfDeath: Double) extends EffectStatus {
    override val effectType: EffectType = EffectType.Poison

    override def getReplayInfo = s"Poison($turnOfDeath)"
  }

  case class Frozen(untilTurn: Double) extends EffectStatus {
    override val effectType: EffectType = EffectType.Freeze

    override def getReplayInfo = s"Frozen($untilTurn)"
  }

  case class Enchanted(untilTurn: Double) extends EffectStatus {
    override val effectType: EffectType = EffectType.Enchanted

    override def getReplayInfo = s"Enchanted($untilTurn)"
  }

  case class WeakEnchanted(untilTurn: Double) extends EffectStatus {
    override val effectType: EffectType = EffectType.WeakEnchanted

    override def getReplayInfo = s"WeakEnchanted($untilTurn)"
  }

  case class Compel(untilTurn: Double, distanceToMove: Distance) extends EffectStatus {
    override val effectType: EffectType = EffectType.Compel

    override def getReplayInfo = s"Compel($untilTurn)"
  }

  case class BlocksAttacksFrom(distances: Set[Distance]) extends EffectStatus {
    override val effectType: EffectType = EffectType.BlockAttacks

    override def getReplayInfo = s"BlocksAttacksFrom"
  }

  case object InstantKillPositional extends EffectStatus {
    override val effectType: EffectType = EffectType.InstantKillPositional
  }

  case class PieceGrowOnPlayerTurn(moraleToPromote: Int, pieceName: String) extends EffectStatus {
    override val effectType: EffectType = EffectType.PieceGrowOnPlayerTurn
  }

  case class PieceGrowOnOpponentTurn(moraleToPromote: Int, pieceName: String) extends EffectStatus {
    override val effectType: EffectType = EffectType.PieceGrowOnOpponentTurn
  }
  }

  case class DecayAfterTurn(turnStarts: Int, moralePerTurn: Int) extends EffectStatus {
    override val effectType: EffectType = EffectType.DecayAfterTurn
  }

}
