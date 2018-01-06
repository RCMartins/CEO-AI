package ceo.play

import ceo.play.EffectStatus._
import ceo.play.Powers._

case class Piece(
  data: PieceData,
  startingPosition: BoardPos,
  pos: BoardPos,
  currentMorale: Int,
  effectStatus: List[EffectStatus]
) {


  override def toString: String = s"${data.name}$pos"

  def team: PlayerTeam = data.team

  def setMorale(morale: Int): Piece = copy(currentMorale = morale)

  def changeMorale(moraleDiff: Int): Piece = copy(currentMorale = currentMorale + moraleDiff)

  private def promoteIfPossible(gameState: GameState): Piece = {
    if (data.canMinionPromote && gameState.getPlayer(data.team.enemy).inBaseRow(pos)) {
      data.powers.collectFirst { case PromoteTo(pieceName) => DataLoader.getPieceData(pieceName, data.team).createPiece(pos) }.get
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

  def afterMeleeKill(startingState: GameState, pieceToKill: Piece): (GameState, Option[Piece]) = {
    val updatedState1 = pieceToKill.checkPlayerLosesMoraleOnDeath(startingState)

    var attackerPieceDies = false

    val updatedState2 =
      if (pieceToKill.data.hasOnMeleeDeathEffects) {
        pieceToKill.data.powers.foldLeft(updatedState1) { (state, power) =>
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
        } // Order of power processing should not matter: a Vampire killing a fireball should still steal 2 morale from enemy
      } else if (data.isGuardian) {
        updatedState1.updatePlayer(startingState.getPlayer(team).updateGuardedPositions(Some(this), None))
      } else { // TODO Use a runner for this:
        var kingPieceLocation: BoardPos = null
        var wrathDuration: Int = 0
        val thisDeathTriggersWrath =
          Distance.adjacentDistances.map(pieceToKill.pos + _).exists { adjacentPos =>
            adjacentPos.getPiece(updatedState1.board).exists { piece =>
              piece.team == pieceToKill.team && piece.data.powers.collectFirst {
                case Powers.TriggerWrathOnAdjacentAllyDeath(turnsToLightUpLocation) =>
                  wrathDuration = turnsToLightUpLocation
                  true
              }.isDefined
            }
          } && {
            val player = updatedState1.getPlayer(pieceToKill.team)
            player.pieces.find(_.data.isKing).orElse(player.piecesAffected.find(_.data.isKing)) match {
              case Some(kingPiece) =>
                kingPieceLocation = kingPiece.pos
                !updatedState1.boardEffects.exists {
                  case BoardEffect.Lightning(boardPos, _) if boardPos == kingPiece.pos => true
                  case _ => false
                }
              case _ => false
            }
          }
        if (thisDeathTriggersWrath) {
          val lightning = BoardEffect.Lightning(kingPieceLocation, updatedState1.currentTurn + wrathDuration)
          updatedState1.copy(boardEffects = lightning :: updatedState1.boardEffects)
        } else {
          updatedState1
        }
      }

    val (updatedState3, updatedPiece) =
      DynamicRunner.foldLeft((updatedState2, this), pieceToKill, data.afterKillRunners)

    if (data.onAnyKillSuicides || attackerPieceDies)
      (updatedState3, None)
    else {
      val (updatedState4, piece) = updatedPiece.moveTo(updatedState3, pieceToKill.pos)
      (updatedState4, Some(piece))
    }
  }

  def afterMagicKill(currentState: GameState, pieceToKill: Piece): (GameState, Option[Piece]) = {
    val updatedState = checkPlayerLosesMoraleOnDeath(currentState)
    val finalState =
      if (data.isGuardian)
        updatedState.updatePlayer(currentState.getPlayer(team).updateGuardedPositions(Some(this), None))
      else
        updatedState
    (finalState, {
      if (data.onAnyKillSuicides)
        None
      else if (data.onMagicPromotes)
        Some(data.powers.collectFirst {
          case PromoteOnSpellCastTo(pieceName) => DataLoader.getPieceData(pieceName, team).createPiece(pos)
        }.get)
      else
        Some(this)
    })
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

  def addEffect(newEffect: EffectStatus): Piece = copy(effectStatus = newEffect :: effectStatus)

  def addAllEffects(newEffects: List[EffectStatus]) = copy(effectStatus = newEffects ++ effectStatus)

  def isPoisoned: Boolean = effectStatus.collectFirst {
    case poison: EffectStatus.Poison => poison
  }.isDefined

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
