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

  private def checkPlayerLosesMoraleOnDeath(currentState: GameState): GameState = {
    data.powers.collectFirst { case PlayerChangeMoraleOnDeath(amount) => amount } match {
      case None => currentState
      case Some(amount) => currentState.changeMorale(team, amount)
    }
  }

  def moveTo(target: BoardPos, currentState: GameState): Piece = {
    copy(pos = target).promoteIfPossible(currentState)
  }

  def afterMeleeKill(pieceToKill: Piece, currentState: GameState): (Option[Piece], GameState) = {
    val updatedState = pieceToKill.checkPlayerLosesMoraleOnDeath(currentState)

    var attackerPieceDies = false
    var updatedThisPiece = this

    val finalState =
      if (pieceToKill.data.hasOnMeleeDeathEffects) {
        pieceToKill.data.powers.foldLeft(updatedState) { (state, power) =>
          power match {
            case OnMeleeDeathKillAttacker =>
              attackerPieceDies = true
              state
            case OnMeleeDeathKillAttackerFromPosition(distances) if distances.contains(pos - pieceToKill.pos) =>
              attackerPieceDies = true
              state
            case OnMeleeDeathSpawnPieces(distances, pieceName) =>
              distances.foldLeft(state) { (state2, dist) =>
                val spawnPos = pos + dist
                if (spawnPos.isEmpty(state2.board))
                  state2.placePiece(DataLoader.getPieceData(pieceName, team).createPiece(spawnPos))
                else state2
              }
            case _ => state
          }
        }
      } else if (data.powers.collectFirst { case OnKillMercenary if pieceToKill.data.isChampion => true }.isDefined) {
        updatedThisPiece = swapTeams
        updatedState.changeMorale(team.enemy, -1)
      } else {
        updatedState
      }

    ( {
      if (data.suicidesOnKill || attackerPieceDies)
        None
      else
        Some(updatedThisPiece.copy(pos = pieceToKill.pos).promoteIfPossible(finalState))
    }, finalState)
  }

  def afterMagicKill(pieceToKill: Piece, currentState: GameState): GameState = {
    pieceToKill.checkPlayerLosesMoraleOnDeath(currentState)
  }

  def afterPoisonDeath(currentState: GameState): GameState = {
    checkPlayerLosesMoraleOnDeath(currentState)
  }

  def afterPoisonPiece(pieceToPoison: Piece, turnsToDeath: Int, currentState: GameState): (GameState, Piece, Piece) = {
    val turnOfDeath = currentState.currentTurn + turnsToDeath
    (currentState, this, pieceToPoison.addEffect(EffectStatus.Poison(turnOfDeath)))
  }

  def petrify(currentState: GameState, turnsPetrified: Int): Piece = {
    copy(effectStatus = Petrified(currentState.currentTurn + turnsPetrified) :: effectStatus)
  }

  def onKillTransformIfPossible(): Piece = {
    data.powers.collectFirst { case OnKillTransformInto(pieceUpgradeName) => pieceUpgradeName } match {
      case None =>
        this
      case Some(pieceUpgradeName) =>
        val pieceData = DataLoader.getPieceData(pieceUpgradeName, data.team)
        Piece(pieceData, pos)
    }
  }

  def swapTeams: Piece =
    copy(data = DataLoader.getPieceData(data.officialName, team.enemy))

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
