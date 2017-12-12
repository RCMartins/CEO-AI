package ceo.play

case class PieceData(
    name: String,
    initialMorale: Int,
    moves: List[Moves],
    powers: List[Powers] = List.empty,
    team: PlayerColor
) {
  override def toString: String = s"$name"
}
