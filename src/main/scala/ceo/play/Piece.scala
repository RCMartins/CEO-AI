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

  def onSuicide(state: GameState): GameState = {
    state
  }

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
    //    val updatedState1 = startingState
    //      if (data.isGuardian) {
    //        startingState.updatePlayer(startingState.getPlayer(team).updateGuardedPositions(Some(this), None))
    //      } else { // TODO Use a gameState level runner for the wrath power:
    //        //        var kingPieceLocation: BoardPos = null
    //        //        var wrathDuration: Int = 0
    //        //        val thisDeathTriggersWrath =
    //        //          Distance.adjacentDistances.map(pieceToKill.pos + _).exists { adjacentPos =>
    //        //            adjacentPos.getPiece(startingState.board).exists { piece =>
    //        //              piece.team == pieceToKill.team && piece.data.powers.collectFirst {
    //        //                case Powers.TriggerWrathOnAdjacentAllyDeath(turnsToLightUpLocation) =>
    //        //                  wrathDuration = turnsToLightUpLocation
    //        //                  true
    //        //              }.isDefined
    //        //            }
    //        //          } && {
    //        //            val player = startingState.getPlayer(pieceToKill.team)
    //        //            player.pieces.find(_.data.isKing).orElse(player.piecesAffected.find(_.data.isKing)) match {
    //        //              case Some(kingPiece) =>
    //        //                kingPieceLocation = kingPiece.pos
    //        //                !startingState.boardEffects.exists {
    //        //                  case BoardEffect.Lightning(boardPos, _) if boardPos == kingPiece.pos => true
    //        //                  case _ => false
    //        //                }
    //        //              case _ => false
    //        //            }
    //        //          }
    //        //        if (thisDeathTriggersWrath) {
    //        //          val lightning = BoardEffect.Lightning(kingPieceLocation, startingState.currentTurn + wrathDuration)
    //        //          startingState.copy(boardEffects = lightning :: startingState.boardEffects)
    //        //        } else {
    //        startingState
    //        //        }
    //      }

    val updatedState1 =
      DynamicRunner.foldLeft(startingState, pieceToKill, pieceToKill.data.afterAnyDeathRunners)

    val (updatedState2, updatedPiece1) =
      DynamicRunner.foldLeft((updatedState1, Some(this)), (this, pieceToKill), pieceToKill.data.afterMeleeDeathRunners)

    val (updatedState3, updatedPiece2) =
      DynamicRunner.foldLeft((updatedState2, updatedPiece1), pieceToKill, data.afterKillRunners)

    updatedPiece2 match {
      case None =>
        (updatedState3, None)
      case Some(finalPiece) =>
        val (updatedState4, piece) = finalPiece.moveTo(updatedState3, pieceToKill.pos)
        (updatedState4, Some(piece))
    }
  }

  def afterMagicKill(startingState: GameState, pieceToKill: Piece): (GameState, Option[Piece]) = {
    val updatedState1 =
      DynamicRunner.foldLeft(startingState, pieceToKill, pieceToKill.data.afterAnyDeathRunners)

    val (updatedState2, updatedPiece) =
      DynamicRunner.foldLeft((updatedState1, Some(this)), pieceToKill, data.afterKillRunners)

    (updatedState2, updatedPiece.map { piece =>
      if (data.onMagicPromotes)
        data.powers.collectFirst {
          case PromoteOnSpellCastTo(pieceName) => DataLoader.getPieceData(pieceName, team).createPiece(pos)
        }.get
      else
        piece
    })
  }

  def afterPoisonDeath(startingState: GameState): GameState = {
    val updatedState =
      DynamicRunner.foldLeft(startingState, this, data.afterAnyDeathRunners)
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

  def swapTeams: Piece =
    copy(data = DataLoader.getPieceData(data.officialName, team.enemy))

  def addEffect(newEffect: EffectStatus): Piece = copy(effectStatus = newEffect :: effectStatus)

  def addAllEffects(newEffects: List[EffectStatus]): Piece = copy(effectStatus = newEffects ++ effectStatus)

  def isPoisoned: Boolean = effectStatus.exists {
    case _: EffectStatus.Poison => true
    case _ => false
  }

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
