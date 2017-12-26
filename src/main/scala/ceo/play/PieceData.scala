package ceo.play

import ceo.play.Powers.{DestroyedBy, GhostMovement, ImmuneTo}

case class PieceData(
  name: String,
  isMinion: Boolean,
  initialMorale: Int,
  moves: List[Moves],
  powers: List[Powers] = List.empty,
  team: PlayerTeam
) {

  override def toString: String = s"$name"

  def createPiece(pos: BoardPos): Piece = Piece(this, pos)

  val simpleName: String = name.takeWhile(c => c.isLetter || c == '-')

  val tier: Int = name.count(_ == '+')

  def nameWithTier: String = s"$simpleName-$tier"

  val isKing: Boolean = name.startsWith("King")

  val isGhost: Boolean = powers.exists {
    case GhostMovement => true
    case _ => false
  }

  val isImmuneTo: Set[EffectStatusType] = powers.flatMap {
    case ImmuneTo(list) => list
    case _ => List.empty
  }.toSet

  val isDestroyedBy: Set[EffectStatusType] = powers.flatMap {
    case DestroyedBy(list) => list
    case _ => List.empty
  }.toSet

}
