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
          DataLoader
            .getPieceData(pieceUpgradeName, data.team)
            .createPiece(pos)
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

  def moveTo(currentState: GameState, target: BoardPos): (GameState, Piece) = {
    val updatedPiece = copy(pos = target).promoteIfPossible(currentState)
    val updatedState =
      if (data.isGuardian)
        currentState.updatePlayer(currentState.getPlayer(team).updateGuardedPositions(Some(this), Some(updatedPiece)))
      else
        currentState
    (updatedState, updatedPiece)
  }

  def afterMeleeKill(currentState: GameState, pieceToKill: Piece): (GameState, Option[Piece]) = {
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
      } else if (data.isGuardian) {
        updatedState.updatePlayer(currentState.getPlayer(team).updateGuardedPositions(Some(this), None))
      } else {
        var kingPieceLocation: BoardPos = null
        var wrathDuration: Int = 0
        val thisDeathTriggersWrath =
          Distance.adjacentDistances.map(pieceToKill.pos + _).exists { adjacentPos =>
            adjacentPos.getPiece(updatedState.board).exists { piece =>
              piece.team == pieceToKill.team && piece.data.powers.collectFirst {
                case Powers.TriggerWrathOnAdjacentAllyDeath(turnsToLightUpLocation) =>
                  wrathDuration = turnsToLightUpLocation
                  true
              }.isDefined
            }
          } && {
            val player = updatedState.getPlayer(pieceToKill.team)
            player.pieces.find(_.data.isKing).orElse(player.piecesAffected.find(_.data.isKing)) match {
              case Some(kingPiece) =>
                kingPieceLocation = kingPiece.pos
                !updatedState.boardEffects.exists {
                  case BoardEffect.Lightning(boardPos, _) if boardPos == kingPiece.pos => true
                  case _ => false
                }
              case _ => false
            }
          }
        if (thisDeathTriggersWrath) {
          val lightning = BoardEffect.Lightning(kingPieceLocation, updatedState.currentTurn + wrathDuration)
          updatedState.copy(boardEffects = lightning :: updatedState.boardEffects)
        } else {
          updatedState
        }
      }

    (finalState, {
      if (data.suicidesOnKill || attackerPieceDies)
        None
      else
        Some(updatedThisPiece.copy(pos = pieceToKill.pos).promoteIfPossible(finalState))
    })
  }

  def afterMagicKill(currentState: GameState, pieceToKill: Piece): GameState = {
    val updatedState = checkPlayerLosesMoraleOnDeath(currentState)
    if (data.isGuardian)
      updatedState.updatePlayer(currentState.getPlayer(team).updateGuardedPositions(Some(this), None))
    else
      updatedState
  }

  def afterPoisonDeath(currentState: GameState): GameState = {
    val updatedState = checkPlayerLosesMoraleOnDeath(currentState)
    if (data.isGuardian)
      updatedState.updatePlayer(currentState.getPlayer(team).updateGuardedPositions(Some(this), None))
    else
      updatedState
  }

  def afterPoisonPiece(pieceToPoison: Piece, turnsToDeath: Int, currentState: GameState): (GameState, Piece, Piece) = {
    val turnOfDeath = currentState.currentTurn + turnsToDeath
    (currentState, this, pieceToPoison.addEffect(EffectStatus.Poison(turnOfDeath)))
  }

  def petrify(currentState: GameState, turnsPetrified: Int): Piece =
    addEffect(Petrified(currentState.currentTurn + turnsPetrified))

  def freeze(currentState: GameState, turnsFrozen: Int): Piece =
    addEffect(Frozen(currentState.currentTurn + turnsFrozen))

  // TODO use this ...
  def onKillTransformIfPossible(): Piece = {
    data.powers.collectFirst { case OnKillTransformInto(pieceUpgradeName) => pieceUpgradeName } match {
      case None =>
        this
      case Some(pieceUpgradeName) =>
        DataLoader
          .getPieceData(pieceUpgradeName, data.team)
          .createPiece(pos)
    }
  }

  def swapTeams: Piece =
    copy(data = DataLoader.getPieceData(data.officialName, team.enemy))

  def addEffect(effect: EffectStatus): Piece = copy(effectStatus = effect :: effectStatus)

  def isPoisoned: Boolean = effectStatus.collectFirst { case poison: EffectStatus.Poison => poison }.isDefined

  def canAct(currentPlayer: Player): Boolean = {
    effectStatus.forall {
      case _: EffectStatus.Petrified => false
      case _: EffectStatus.Frozen => false
      case _ => true
    } && {
      !data.canOnlyActAfterPieceLost || currentPlayer.extraData.fallenPiecesPositions.nonEmpty
    }
  }

}
