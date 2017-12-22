package ceo.play

case class PieceData(
    name: String,
    isMinion: Boolean,
    initialMorale: Int,
    moves: List[Moves],
    powers: List[Powers] = List.empty,
    team: PlayerTeam
) {

  override def toString: String = s"$name"

  val isKing: Boolean = name.startsWith("King")

  def createPiece(pos: BoardPos): Piece = Piece(this, pos)
}
