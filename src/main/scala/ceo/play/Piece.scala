package ceo.play

import ceo.play.Powers._

case class Piece(
  data: PieceData,
  startingPosition: BoardPos,
  pos: BoardPos,
  currentMorale: Int,
  effectStatus: List[EffectStatus]
) {

  def onTransform(startingState: GameState): GameState = {
    // TODO I think this has do be done to simulate a bug of v0.52:
    // Example: Vampire transforms into bat -> Dove move triggers, but technically no piece died ...

    val (updatedState, _) =
      DynamicRunner.foldLeft((startingState, None), this, data.afterAnyDeathRunners)

    updatedState
  }

  override def toString: String = s"${data.name}$pos"

  @inline final def team: PlayerTeam = data.team

  def setMorale(morale: Int): Piece = copy(currentMorale = morale)

  def changeMorale(moraleDiff: Int): Piece = copy(currentMorale = Math.max(0, currentMorale + moraleDiff)) // TODO: I'm not sure if there is a 100 max ...

  private def promoteIfPossible(gameState: GameState): Piece = { // TODO remove this and put it in afterPieceMovesRunners
    if (data.canMinionPromote && gameState.getPlayer(team.enemy).inBaseRow(pos)) {
      data.powers.collectFirst { case PromoteTo(pieceName) => DataLoader.getPieceData(pieceName, team).createPiece(pos) }.get
    } else {
      this
    }
  }

  def moveTo(currentState: GameState, target: BoardPos, isFromPlayerMove: Boolean = false): (GameState, Option[Piece]) = {
    val updatedPiece1 = copy(pos = target).promoteIfPossible(currentState)
    val updatedState1 =
      if (data.isGuardian)
        currentState.updatePlayer(currentState.getPlayer(team).updateGuardedPositions(Some(this), Some(updatedPiece1)))
      else
        currentState

    val (updatedState2, updatedPiece2) =
      DynamicRunner.foldLeft((updatedState1, Some(updatedPiece1)), (updatedPiece1, target - this.pos, isFromPlayerMove), data.afterPieceMovesRunners)

    (updatedState2, updatedPiece2)
  }

  def afterMeleeKill(startingState: GameState, pieceToKill: Piece): (GameState, Option[Piece]) = {
    val (updatedState1, updatedPiece1) =
      DynamicRunner.foldLeft((startingState, Some(this)), pieceToKill, pieceToKill.data.afterAnyDeathRunners)

    val (updatedState2, updatedPiece2) =
      DynamicRunner.foldLeft((updatedState1, updatedPiece1), (this, pieceToKill), pieceToKill.data.afterMeleeDeathRunners)

    val (updatedState3, updatedPiece3) =
      DynamicRunner.foldLeft((updatedState2, updatedPiece2), pieceToKill, data.afterKillRunners)

    val updatedState4 =
      DynamicRunner.foldLeft(updatedState3, pieceToKill, updatedState3.getPlayers.flatMap(_.extraData.globalDeathPieceRunners))

    updatedPiece3 match {
      case None =>
        (updatedState4, None)
      case Some(finalPiece) =>
        val (updatedState5, piece) = finalPiece.moveTo(updatedState4, pieceToKill.pos)
        (updatedState5.updatePlayer(pieceToKill.team.color, _.pieceDied(pieceToKill)), piece) // TODO VERY Hard coded .......
    }
  }

  def afterMagicKill(startingState: GameState, pieceToKill: Piece): (GameState, Option[Piece]) = {
    val (updatedState1, updatedPiece1) =
      DynamicRunner.foldLeft((startingState, Some(this)), pieceToKill, pieceToKill.data.afterAnyDeathRunners)

    val (updatedState2, updatedPiece2) =
      DynamicRunner.foldLeft((updatedState1, updatedPiece1), pieceToKill, data.afterKillRunners)

    val updatedState3 =
      DynamicRunner.foldLeft(updatedState2, pieceToKill, updatedState2.getPlayers.flatMap(_.extraData.globalDeathPieceRunners))

    updatedPiece2 match {
      case Some(piece) => piece.afterMagicCast(updatedState3, Some(pieceToKill))
      case None => (updatedState3, None)
    }
  }

  def afterMagicCast(startingState: GameState, pieceAttacked: Option[Piece]): (GameState, Option[Piece] /* this piece */ ) = {
    DynamicRunner.foldLeft((startingState, Some(this)), (this, pieceAttacked), data.afterMagicCastRunners)
  }

  def afterPoisonDeath(startingState: GameState): GameState = {
    val (updatedState1, _) =
      DynamicRunner.foldLeft((startingState, None), this, data.afterAnyDeathRunners)

    val updatedState2 =
      DynamicRunner.foldLeft(updatedState1, this, updatedState1.getPlayers.flatMap(_.extraData.globalDeathPieceRunners))

    updatedState2
  }

  // TODO: change this functions to static ones and make "this" an Option[Piece] so that effects can be used by death pieces like 'Comet'
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

  def compelPiece(
    currentState: GameState,
    pieceToWeakEnchant: Piece,
    turnsCompelled: Int
  ): (Option[Piece] /* attacker piece updated */ , Option[Piece] /* affected piece updated */ ) = {
    val distanceToMove = (pos - pieceToWeakEnchant.pos).toUnitVector
    val effectStatus = EffectStatus.Compel(currentState.currentTurn + turnsCompelled, distanceToMove)
    applyStatusEffectToPiece(currentState, pieceToWeakEnchant, effectStatus)
  }

  private def applyStatusEffectToPiece(
    currentState: GameState,
    pieceToAffect: Piece,
    effectStatus: EffectStatus
  ): (Option[Piece] /* attacker piece updated */ , Option[Piece] /* affected piece updated */ ) = {
    val updatedPieceToPoison: Option[Piece] =
      if (pieceToAffect.data.isDestroyedBy(effectStatus.effectType)) {
        None
      } else {
        Some(pieceToAffect.addEffect(effectStatus))
      }

    val attackerPieceUpdatedOption1: Option[Piece] =
      DynamicRunner.foldLeft(Some(this), (currentState, effectStatus), pieceToAffect.data.attackerUsesStatusEffectRunners)

    val attackerPieceUpdatedOption2 =
      attackerPieceUpdatedOption1 match {
        case None => None
        case Some(attackerPiece) =>
          attackerPiece.afterMagicCast(currentState, updatedPieceToPoison)._2 // TODO does this simplification works always?
      }

    (attackerPieceUpdatedOption2, updatedPieceToPoison)
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

  def isPetrified: Boolean = effectStatus.exists {
    case _: EffectStatus.Petrified => true
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
        case EffectStatus.BlocksAttacksFrom(distances) =>
          distances(target - pos)
        case _ => false
      }
    }
  }

  def removeBlockEffect: Piece = copy(effectStatus = effectStatus.filterNot(_.effectType == EffectType.BlockAttacks))

  def getReplayInfo(withPos: Boolean, withTeam: Boolean): String = {
    s"${if (withPos) pos.toReplayInfo else ""}${if (withTeam) data.nameWithPlayerBase else data.officialName}[$currentMorale]" + {
      if (effectStatus.isEmpty)
        ""
      else
        s"(${effectStatus.map(_.getReplayInfo).sorted.mkString(",")})"
    }
  }
}
