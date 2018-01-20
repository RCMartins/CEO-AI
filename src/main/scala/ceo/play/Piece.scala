package ceo.play

import ceo.play.Powers._

case class Piece(
  data: PieceData,
  startingPosition: BoardPos,
  pos: BoardPos,
  currentMorale: Int,
  effectStatus: List[EffectStatus]
) {
  def afterMagicCast(startingState: GameState): (GameState, Option[Piece]) = {
    DynamicRunner.foldLeft((startingState, Some(this)), this, data.afterMagicCastRunners)
  }

  def onSuicide(state: GameState): GameState = {
    // TODO I think dove power triggers on this!
    state
  }

  override def toString: String = s"${data.name}$pos"

  @inline final def team: PlayerTeam = data.team

  def setMorale(morale: Int): Piece = copy(currentMorale = morale)

  def changeMorale(moraleDiff: Int): Piece = copy(currentMorale = currentMorale + moraleDiff)

  private def promoteIfPossible(gameState: GameState): Piece = {
    if (data.canMinionPromote && gameState.getPlayer(team.enemy).inBaseRow(pos)) {
      data.powers.collectFirst { case PromoteTo(pieceName) => DataLoader.getPieceData(pieceName, team).createPiece(pos) }.get
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

    val (updatedState1, updatedPiece1) =
      DynamicRunner.foldLeft((startingState, Some(this)), pieceToKill, pieceToKill.data.afterAnyDeathRunners)

    val (updatedState2, updatedPiece2) =
      DynamicRunner.foldLeft((updatedState1, updatedPiece1), (this, pieceToKill), pieceToKill.data.afterMeleeDeathRunners)

    val (updatedState3, updatedPiece3) =
      DynamicRunner.foldLeft((updatedState2, updatedPiece2), pieceToKill, data.afterKillRunners)

    val updatedState4 =
      DynamicRunner.foldLeft(updatedState3, pieceToKill, updatedState3.gameRunner.globalPieceDeathRunners)

    updatedPiece3 match {
      case None =>
        (updatedState4, None)
      case Some(finalPiece) =>
        val (updatedState5, piece) = finalPiece.moveTo(updatedState4, pieceToKill.pos)
        (updatedState5, Some(piece))
    }
  }

  def afterMagicKill(startingState: GameState, pieceToKill: Piece): (GameState, Option[Piece]) = {
    val (updatedState1, updatedPiece1) =
      DynamicRunner.foldLeft((startingState, Some(this)), pieceToKill, pieceToKill.data.afterAnyDeathRunners)

    val (updatedState2, updatedPiece2) =
      DynamicRunner.foldLeft((updatedState1, updatedPiece1), pieceToKill, data.afterKillRunners)

    val updatedState3 =
      DynamicRunner.foldLeft(updatedState2, pieceToKill, updatedState2.gameRunner.globalPieceDeathRunners)

    updatedPiece2 match {
      case Some(piece) => piece.afterMagicCast(updatedState3)
      case None => (updatedState3, None)
    }
  }

  def afterPoisonDeath(startingState: GameState): GameState = {
    val (updatedState1, _) =
      DynamicRunner.foldLeft((startingState, None), this, data.afterAnyDeathRunners)

    val updatedState2 =
      DynamicRunner.foldLeft(updatedState1, this, updatedState1.gameRunner.globalPieceDeathRunners)

    updatedState2
  }

  def poisonPiece(
    currentState: GameState,
    pieceToPoison: Piece,
    turnsToDeath: Int
  ): (Option[Piece] /* attacker piece updated */ , Option[Piece] /* affected piece updated */ ) = {
    val effectStatus = EffectStatus.Poison(currentState.currentTurn + turnsToDeath)
    applyStatusEffectToPiece(currentState, pieceToPoison, effectStatus)
  }

  def petrifyPiece(
    currentState: GameState,
    pieceToPetrify: Piece,
    turnsPetrified: Int
  ): (Option[Piece] /* attacker piece updated */ , Option[Piece] /* affected piece updated */ ) = {
    val effectStatus = EffectStatus.Petrified(currentState.currentTurn + turnsPetrified)
    applyStatusEffectToPiece(currentState, pieceToPetrify, effectStatus)
  }

  def freezePiece(
    currentState: GameState,
    pieceToFreeze: Piece,
    turnsFrozen: Int
  ): (Option[Piece] /* attacker piece updated */ , Option[Piece] /* affected piece updated */ ) = {
    val effectStatus = EffectStatus.Frozen(currentState.currentTurn + turnsFrozen)
    applyStatusEffectToPiece(currentState, pieceToFreeze, effectStatus)
  }

  def enchantPiece(
    currentState: GameState,
    pieceToEnchant: Piece,
    turnsEnchanted: Int
  ): (Option[Piece] /* attacker piece updated */ , Option[Piece] /* affected piece updated */ ) = {
    val effectStatus = EffectStatus.Enchanted(currentState.currentTurn + turnsEnchanted)
    applyStatusEffectToPiece(currentState, pieceToEnchant, effectStatus)
  }

  def weakEnchantPiece(
    currentState: GameState,
    pieceToWeakEnchant: Piece,
    turnsWeakEnchanted: Int
  ): (Option[Piece] /* attacker piece updated */ , Option[Piece] /* affected piece updated */ ) = {
    val effectStatus = EffectStatus.WeakEnchanted(currentState.currentTurn + turnsWeakEnchanted)
    applyStatusEffectToPiece(currentState, pieceToWeakEnchant, effectStatus)
  }

  private def applyStatusEffectToPiece(
    currentState: GameState,
    pieceToAffect: Piece,
    effectStatus: EffectStatus
  ): (Option[Piece] /* attacker piece updated */ , Option[Piece] /* affected piece updated */ ) = {
    val updatedPieceToPoison: Option[Piece] =
      if (pieceToAffect.data.isDestroyedBy(EffectType.Poison)) {
        None
      } else {
        Some(pieceToAffect.addEffect(effectStatus))
      }

    val updatedAttackerPiece =
      DynamicRunner.foldLeft(Some(this), (currentState, effectStatus), pieceToAffect.data.attackerUsesStatusEffectRunners)

    (updatedAttackerPiece, updatedPieceToPoison)
  }

  def swapTeams: Piece =
    copy(data = DataLoader.getPieceData(data.officialName, team.enemy))

  def addEffect(newEffect: EffectStatus): Piece = copy(effectStatus = newEffect :: effectStatus)

  def addAllEffects(newEffects: List[EffectStatus]): Piece = copy(effectStatus = newEffects ++ effectStatus)

  def isPoisoned: Boolean = effectStatus.exists {
    case _: EffectStatus.Poison => true
    case _ => false
  }

  def isEnchanted: Boolean = effectStatus.exists {
    case _: EffectStatus.Enchanted => true
    case _ => false
  }

  def isWeakEnchanted: Boolean = effectStatus.exists {
    case _: EffectStatus.WeakEnchanted => true
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

  def canBlockFrom(target: BoardPos): Boolean = {
    data.isABlockerPiece && {
      effectStatus.exists {
        case EffectStatus.BlocksAttacksFrom(distances) => distances(target - pos)
        case _ => false
      }
    }
  }

  def removeBlockEffect: Piece = copy(effectStatus = effectStatus.filterNot(_.effectType == EffectType.BlockAttacks))

}
