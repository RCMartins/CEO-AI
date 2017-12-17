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

  private def loseMoraleOnDeath(currentState: GameState): GameState = {
    data.powers.collectFirst { case LoseMoraleOnDeath(amount) => amount } match {
      case None => currentState
      case Some(amount) => currentState.changeMorale(team, -amount)
    }
  }

  def moveTo(target: BoardPos, currentState: GameState): Piece = {
    copy(pos = target).promoteIfPossible(currentState)
  }

  def killed(pieceToKill: Piece, currentState: GameState): (Piece, GameState) = {
    val updatedState = pieceToKill.loseMoraleOnDeath(currentState)
    (copy(pos = pieceToKill.pos).promoteIfPossible(updatedState), updatedState)
  }

  def destroyed(currentState: GameState): GameState = {
    loseMoraleOnDeath(currentState)
  }
}

object Piece {
  def apply(data: PieceData, pos: BoardPos): Piece = Piece(data, pos, data.initialMorale, hasMoved = false)
}