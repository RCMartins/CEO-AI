package ceo.play

import ceo.play.EffectStatus._

case class Piece(
  data: PieceData,
  startingPosition: BoardPos,
  pos: BoardPos,
  currentMorale: Int,
  effectStatus: List[EffectStatus]
) {

  import ceo.play.Powers._

  override def toString: String = s"${data.name}$pos"

  def team: PlayerTeam = data.team

  def setMorale(morale: Int): Piece = copy(currentMorale = morale)

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

  private def loseMoraleOnDeathIfPossible(currentState: GameState): GameState = {
    data.powers.collectFirst { case LoseMoraleOnDeath(amount) => amount } match {
      case None => currentState
      case Some(amount) => currentState.changeMorale(team, -amount)
    }
  }

  def moveTo(target: BoardPos, currentState: GameState): Piece = {
    copy(pos = target).promoteIfPossible(currentState)
  }

  def afterMeleeKill(pieceToKill: Piece, currentState: GameState): (Piece, GameState) = {
    val updatedState = pieceToKill.loseMoraleOnDeathIfPossible(currentState)
    (copy(pos = pieceToKill.pos).promoteIfPossible(updatedState), updatedState)
  }

  def afterMagicKill(pieceToKill: Piece, currentState: GameState): GameState = {
    pieceToKill.loseMoraleOnDeathIfPossible(currentState)
  }

  def afterPoisonDeath(currentState: GameState): GameState = {
    loseMoraleOnDeathIfPossible(currentState)
  }

  def afterPoisonPiece(pieceToPoison: Piece, turnsToDeath: Int, currentState: GameState): (GameState, Piece, Piece) = {
    val turnOfDeath = currentState.currentTurn + turnsToDeath
    (currentState, this, pieceToPoison.addEffect(EffectStatus.Poison(turnOfDeath)))
  }

  def petrify(currentState: GameState, turnsPetrified: Int): Piece = {
    copy(effectStatus = Petrified(currentState.currentTurn + turnsPetrified) :: effectStatus)
  }

  def addEffect(effect: EffectStatus): Piece = copy(effectStatus = effect :: effectStatus)

  def isPoisoned: Boolean = effectStatus.collectFirst { case poison: EffectStatus.Poison => poison }.isDefined

  def isNotFrozenOrPetrified: Boolean = {
    effectStatus.forall {
      case _: EffectStatus.Petrified => false
      case _: EffectStatus.Frozen => false
      case _ => true
    }
  }

}

object Piece {
  def apply(data: PieceData, pos: BoardPos): Piece =
    Piece(data, pos, pos, data.initialMorale, effectStatus = List.empty)
}
