package ceo.play

case class Piece(data: PieceData, pos: BoardPos, currentMorale: Int, hasMoved: Boolean) {

  import ceo.play.Powers._

  override def toString: String = s"${data.name}$pos"

  def team: PlayerTeam = data.team

  private def promoteIfPossible(gameState: GameState): Piece = {
    if (gameState.getPlayer(data.team.enemy).inBaseRow(pos)) {
      data.powers.collectFirst { case PromoteTo(pieceUpgradeName) => pieceUpgradeName } match {
        case None =>
          this
        case Some(pieceUpgradeName) =>
          val pieceData = DataLoader.getPieceData(pieceUpgradeName, data.team)
          Piece(pieceData, pos)
      }
    } else {
      this
    }
  }

  def moveTo(target: BoardPos, gameState: GameState): Piece = {
    copy(pos = target).promoteIfPossible(gameState)
  }

  def killed(pieceToKill: Piece, currentState: GameState): (Piece, GameState) = {
    val updatedState =
      pieceToKill.data.powers.collectFirst { case LoseMoraleOnDeath(amount) => amount } match {
        case None => currentState
        case Some(amount) => currentState.changeMorale(pieceToKill.team, -amount)
      }
    (copy(pos = pieceToKill.pos).promoteIfPossible(updatedState), updatedState)
  }
}

object Piece {
  def apply(data: PieceData, pos: BoardPos): Piece = Piece(data, pos, data.initialMorale, hasMoved = false)
}