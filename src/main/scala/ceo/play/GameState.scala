package ceo.play

import ceo.play.Moves.UnstoppableTeleportTransformInto
import ceo.play.PlayerTeam.{Black, White}
import com.softwaremill.quicklens._

case class GameState(
  board: Board,
  playerWhite: Player,
  playerBlack: Player,
  currentTurn: Double,
  movesHistory: List[PlayerMove],
  boardEffects: List[BoardEffect],
  gameRunner: GameRunner,
  endOfTurnActions: List[EndOfTurnAction]
) {

  def allPieces: List[Piece] = playerWhite.allPieces ++ playerBlack.allPieces

  def trimMorale: GameState = {
    if (playerWhite.morale < 0 || playerBlack.morale < 0) {
      copy(
        playerWhite = playerWhite.copy(morale = Math.max(0, playerWhite.morale)),
        playerBlack = playerBlack.copy(morale = Math.max(0, playerBlack.morale))
      )
    } else {
      this
    }
  }

  def getPlayer(team: PlayerTeam): Player = if (team == White) playerWhite else playerBlack

  def nextTurn: GameState = copy(currentTurn = currentTurn + 0.5)

  override def toString: String = {
    val nameSize = 18
    val normalDashLine = " " + "-" * ((nameSize + 1) * 8 + 1)

    def moraleDashLine(morale: Int) = {
      val turnText = s"----- turn: $currentTurn[${getCurrentPlayer.team.letter}]"
      val textInLine = s" morale: ${morale.toString} "
      val firstHalf = normalDashLine.length / 2 - turnText.length - textInLine.length / 2 - 1
      val secondHalf = normalDashLine.length - firstHalf - turnText.length - textInLine.length - 1
      s" $turnText${"-" * firstHalf}$textInLine${"-" * secondHalf}\n"
    }

    board.getRows.zipWithIndex.map { case (line, rowN) => line.map { pieceOpt =>
      val formatStr = s"%${nameSize}s"
      formatStr.format(
        pieceOpt.map(
          piece => piece.data.name).getOrElse(""))
    }.mkString(s"$rowN|", "|", "|")
    }
      .mkString(moraleDashLine(playerBlack.morale), "\n" + normalDashLine + "\n", "\n" + moraleDashLine(playerWhite.morale))
  }

  def getBoardPieceNames: String = {
    val pieceNames =
      board.getRows.map(_.map(_.map(_.data.name).getOrElse("e"))).toVector

    val columnLengths: Seq[Int] =
      for (column <- 0 until 8) yield Math.max(8, (0 until 8).map(row => pieceNames(row)(column).length).max)

    val sb = new StringBuilder
    for (row <- 0 until 8; column <- 0 until 8) {
      if (column == 7)
        sb.append(pieceNames(row)(column) + "\n")
      else {
        val name = pieceNames(row)(column)
        sb.append(name)
        sb.append(" " * (1 + columnLengths(column) - name.length))
      }
    }
    sb.toString()
  }

  /**
    * Removes piece and adds another piece and updates morale/player pieces
    * updatePiece(piece1, piece2) === removePiece(piece1).placePiece(piece2)
    */
  def updatePiece(piece: Piece, pieceNewPos: Piece): GameState = {
    if (piece eq pieceNewPos)
      this
    else
      removePiece(piece).placePiece(pieceNewPos)
  }

  /**
    * Removes piece and adds another piece if it exists and updates morale/player pieces
    */
  def updatePieceIfAlive(piece: Piece, pieceNewPosOption: Option[Piece]): GameState =
    pieceNewPosOption match {
      case Some(pieceNewPos) =>
        updatePiece(piece, pieceNewPos)
      case None =>
        removePiece(piece)
    }

  def updatePlayer(playerUpdated: Player): GameState = {
    if (playerUpdated.team == White)
      copy(playerWhite = playerUpdated)
    else
      copy(playerBlack = playerUpdated)
  }

  /**
    * Places piece and updates morale/player pieces
    */
  def placePiece(piece: Piece): GameState = {
    val withNewPiece = copy(board = board.place(piece))

    if (piece.team == White) {
      withNewPiece.copy(playerWhite =
        withNewPiece.playerWhite
          .changeMorale(piece.currentMorale)
          .placePiece(piece)
      )
    } else {
      withNewPiece.copy(playerBlack =
        withNewPiece.playerBlack
          .changeMorale(piece.currentMorale)
          .placePiece(piece)
      )
    }
  }

  def placePieceIfAlive(pieceOption: Option[Piece]): GameState = {
    pieceOption match {
      case None => this
      case Some(piece) => placePiece(piece)
    }
  }

  /**
    * Removes piece and updates morale/player pieces
    */
  def removePiece(piece: Piece): GameState = {
    val withoutPiece = copy(board = board.remove(piece.pos))

    if (piece.team == White) {
      withoutPiece.copy(playerWhite =
        withoutPiece.playerWhite
          .changeMorale(-piece.currentMorale)
          .removePiece(piece)
      )
    } else {
      withoutPiece.copy(playerBlack =
        withoutPiece.playerBlack
          .changeMorale(-piece.currentMorale)
          .removePiece(piece)
      )
    }
  }

  def doActionIfCondition(condition: Boolean, function: GameState => GameState): GameState = {
    if (condition) function(this) else this
  }

  def changeMorale(playerTeam: PlayerTeam, moraleDiff: Int): GameState = {
    if (playerTeam == White)
      copy(playerWhite = playerWhite.changeMorale(moraleDiff))
    else
      copy(playerBlack = playerBlack.changeMorale(moraleDiff))
  }

  def getCurrentPlayerMoves: List[PlayerMove] = {
    val currentPlayer: Player = getCurrentPlayer

    val allMoves =
      (currentPlayer.pieces ++ currentPlayer.piecesAffected).flatMap { piece =>
        if (piece.canAct(currentPlayer))
          piece.data.moves.flatMap {
            case single: SingleMove =>
              single.getValidMove(piece, this, currentPlayer)
            case multi: MultipleMoves =>
              multi.getValidMoves(piece, this, currentPlayer)
            case _ =>
              ???
          }
        else if (piece.data.hasUnstoppableMoves) {
          piece.data.moves.flatMap {
            case move: UnstoppableTeleportTransformInto => move.getValidMove(piece, this, currentPlayer)
            case _ =>
              List.empty
          }
        } else
          List.empty
      }

    val (attacks, others) = allMoves.partition {
      case _: PlayerMove.Attack => true
      case _ => false
    }

    attacks ++ others
  }

  def generateAllNextStates: List[GameState] = {
    getCurrentPlayerMoves.map(move => playPlayerMove(move, turnUpdate = true))
  }

  def winner: PlayerWinType = {
    if (playerWhite.morale == 0) {
      if (playerBlack.morale == 0) {
        PlayerWinType.Draw
      } else {
        PlayerWinType.PlayerBlack
      }
    } else if (playerBlack.morale == 0) {
      PlayerWinType.PlayerWhite
    } else {
      PlayerWinType.NotFinished
    }
  }

  def playPlayerMove(move: PlayerMove, turnUpdate: Boolean): GameState = {
    import PlayerMove._

    def guardianSwapPiece(state: GameState, attackerPiece: Piece, piece: Piece): (GameState, Piece) = {
      if (attackerPiece.team != piece.team && piece.data.isChampion && !piece.data.isImmuneTo(EffectType.Displacement)) {
        state.getPlayer(piece.team).extraData.guardedPositions.get(piece.pos) match {
          case Some(guardianPiece) =>
            val updatedState = playPlayerMove(PlayerMove.Swap(piece, guardianPiece), turnUpdate = false)
            (updatedState, piece.pos.getPiece(updatedState.board).get)
          case None =>
            (state, piece)
        }
      } else {
        (state, piece)
      }
    }

    val newState: GameState = move match {
      // Standard moves:
      case Move(piece, target) =>
        val (updatedState, pieceNewPos) = piece.moveTo(this, target, isFromPlayerMove = true)
        updatedState
          .updatePieceIfAlive(piece, pieceNewPos)
      case Attack(piece, _pieceToKill) =>
        val (updatedState1, pieceToKill) = guardianSwapPiece(this, piece, _pieceToKill)
        val (updatedState2, pieceNewPosOption) = piece.afterMeleeKill(updatedState1, pieceToKill)
        updatedState2
          .removePiece(pieceToKill)
          .updatePieceIfAlive(piece, pieceNewPosOption)
      case AttackCanBeBlocked(piece, _pieceToAttack) =>
        val (updatedState, pieceToAttack) = guardianSwapPiece(this, piece, _pieceToAttack)
        if (pieceToAttack == _pieceToAttack) {
          val pieceToKillUpdated = pieceToAttack.removeBlockEffect
          updatedState
            .updatePiece(pieceToAttack, pieceToKillUpdated)
        } else {
          playPlayerMove(Attack(piece, pieceToAttack), turnUpdate = false)
        }
      case Swap(piece, pieceToSwap) =>
        val updatedState1 = this.removePiece(piece).removePiece(pieceToSwap)

        val (updatedState2, pieceToSwapUpdated) = pieceToSwap.moveTo(updatedState1, piece.pos)
        val (updatedState3, pieceUpdated) = piece.moveTo(updatedState2, pieceToSwap.pos)

        updatedState3
          .placePieceIfAlive(pieceUpdated)
          .placePieceIfAlive(pieceToSwapUpdated)
      // Ranged moves:
      case RangedDestroy(piece, _pieceToDestroy) =>
        val (updatedState1, pieceToDestroy) = guardianSwapPiece(this, piece, _pieceToDestroy)
        val (updatedState2, pieceUpdatedOption) = piece.afterMagicKill(updatedState1, pieceToDestroy)
        updatedState2
          .removePiece(pieceToDestroy)
          .updatePieceIfAlive(piece, pieceUpdatedOption)
      case RangedPetrify(piece, _pieceToPetrify, turnsPetrified) =>
        val (updatedState, pieceToPetrify) = guardianSwapPiece(this, piece, _pieceToPetrify)
        val (pieceUpdatedOption, pieceToPetrifyUpdatedOption) = piece.petrifyPiece(updatedState, pieceToPetrify, turnsPetrified)
        updatedState
          .updatePieceIfAlive(pieceToPetrify, pieceToPetrifyUpdatedOption)
          .updatePieceIfAlive(piece, pieceUpdatedOption)
      case RangedPush(piece, _pieceToPush, moraleCost, maxPushDistance) =>
        val (updatedState1, pieceToPush) = guardianSwapPiece(this, piece, _pieceToPush)
        val pieceToPushPos = pieceToPush.pos
        val dir = (pieceToPushPos - piece.pos).toUnitVector
        val positions = BoardPos.List1to8.view(0, maxPushDistance)
          .map(distance => pieceToPushPos + dir * distance)
          .takeWhile(_.isEmpty(board))

        val (updatedState2, pieceToPushUpdated) = pieceToPush.moveTo(updatedState1, positions.last)
        val (updatedState3, pieceUpdated) = piece.afterMagicCast(updatedState2, pieceToPushUpdated)
        updatedState3
          .changeMorale(piece.team, -moraleCost)
          .updatePieceIfAlive(pieceToPush, pieceToPushUpdated)
          .updatePieceIfAlive(piece, pieceUpdated)
      case RangedPushSpawn(piece, pieceToPush, moraleCost, maxPushDistance, pieceData) =>
        this
          .playPlayerMove(PlayerMove.RangedPush(piece, pieceToPush, moraleCost, maxPushDistance), turnUpdate = false)
          .addEndOfTurnPiece(pieceData.createPiece(pieceToPush.pos))
      // Magic moves:
      case MagicDestroy(piece, _pieceToDestroy) =>
        val (updatedState1, pieceToDestroy) = guardianSwapPiece(this, piece, _pieceToDestroy)
        val (updatedState2, pieceUpdatedOption) = piece.afterMagicKill(updatedState1, pieceToDestroy)
        updatedState2
          .removePiece(pieceToDestroy)
          .updatePieceIfAlive(piece, pieceUpdatedOption)
      case MagicPoison(piece, _pieceToPoison, turnsToDeath) =>
        val (updatedState, pieceToPoison) = guardianSwapPiece(this, piece, _pieceToPoison)
        val (updatedPieceOption, pieceToPoisonUpdatedOption) = piece.poisonPiece(updatedState, pieceToPoison, turnsToDeath)
        updatedState
          .updatePieceIfAlive(pieceToPoison, pieceToPoisonUpdatedOption)
          .updatePieceIfAlive(piece, updatedPieceOption)
      case MagicCharm(piece, _pieceToCharm) =>
        val (updatedState, pieceToCharm) = guardianSwapPiece(this, piece, _pieceToCharm)
        val pieceToCharmUpdated = pieceToCharm.swapTeams
        updatedState
          .updatePiece(pieceToCharm, pieceToCharmUpdated)
      case TransformEnemyIntoAllyPiece(piece, _pieceToTransform, moraleCost, allyPieceData) =>
        val (updatedState1, pieceToTransform) = guardianSwapPiece(this, piece, _pieceToTransform)
        val (updatedState2, pieceUpdatedOption) = piece.afterMagicKill(updatedState1, pieceToTransform)
        val newTransformedPiece = allyPieceData.createPiece(pieceToTransform.pos)
        updatedState2
          .changeMorale(piece.team, -moraleCost)
          .updatePiece(pieceToTransform, newTransformedPiece)
          .updatePieceIfAlive(piece, pieceUpdatedOption)
      case KingCastling(kingPiece, allyPiece, kingTarget, allyTarget) =>
        val kingWithoutCastling = DataLoader
          .getPieceData("King-no-Cas", kingPiece.team) // TODO make this a StatusEffect instead of a new piece?
          .createPiece(kingTarget)
          .setMorale(kingPiece.currentMorale)
          .addAllEffects(kingPiece.effectStatus)
        this
          .updatePiece(allyPiece, allyPiece.copy(pos = allyTarget))
          .updatePiece(kingPiece, kingWithoutCastling)
      case TeleportPiece(piece, _pieceToTeleport, target, _, _) =>
        val (updatedState1, pieceToTeleport) = guardianSwapPiece(this, piece, _pieceToTeleport)
        val (updatedState2, pieceToTeleportUpdated) = pieceToTeleport.moveTo(updatedState1, target)
        updatedState2
          .updatePieceIfAlive(pieceToTeleport, pieceToTeleportUpdated)
      case MagicPush(piece, _pieceToPush, moraleCost, maxPushDistance) =>
        val (updatedState1, pieceToPush) = guardianSwapPiece(this, piece, _pieceToPush)
        val pieceToPushPos = pieceToPush.pos
        val dirDist = pieceToPushPos - piece.pos
        val targetFinalPosition = {
          val absRow = Math.abs(dirDist.rowDiff)
          val absColumn = Math.abs(dirDist.columnDiff)
          val dir =
            if (absRow == absColumn)
              dirDist.toUnitVector
            else if (absRow > absColumn)
              dirDist.setColumn(0).toUnitVector
            else
              dirDist.setRow(0).toUnitVector
          val positions = BoardPos.List1to8.view(0, maxPushDistance)
            .map(distance => pieceToPushPos + dir * distance)
            .takeWhile(_.isEmpty(board))
          positions.last
        }

        val (updatedState2, pieceToPushUpdated) = pieceToPush.moveTo(updatedState1, targetFinalPosition)
        updatedState2
          .changeMorale(piece.team, -moraleCost)
          .updatePieceIfAlive(pieceToPush, pieceToPushUpdated)
      case MagicFreeze(piece, _pieceToFreeze, freezeDuration) =>
        val (updatedState, pieceToFreeze) = guardianSwapPiece(this, piece, _pieceToFreeze)
        val (pieceUpdatedOption, pieceToFreezeUpdatedOption) = piece.freezePiece(updatedState, pieceToFreeze, freezeDuration)
        updatedState
          .updatePieceIfAlive(pieceToFreeze, pieceToFreezeUpdatedOption)
          .updatePieceIfAlive(piece, pieceUpdatedOption)
      case MagicPushFreeze(piece, _pieceToPushFreeze, maxPushDistance, freezeDuration) =>
        val (updatedState, pieceToPushFreeze) = guardianSwapPiece(this, piece, _pieceToPushFreeze)
        // TODO for now assume it isn't possible for the piece to die after using the MagicPushFreeze (the piece would need to have a non-official-game power combination)
        val (Some(pieceUpdated), pieceToPushFreezeUpdatedOption) = piece.freezePiece(updatedState, pieceToPushFreeze, freezeDuration)
        updatedState
          .updatePieceIfAlive(pieceToPushFreeze, pieceToPushFreezeUpdatedOption)
          .updatePiece(piece, pieceUpdated)
          .doActionIfCondition(pieceToPushFreezeUpdatedOption.isDefined,
            _.playPlayerMove(PlayerMove.MagicPush(pieceUpdated, pieceToPushFreezeUpdatedOption.get, 0, maxPushDistance), turnUpdate = false))
      case MagicLightning(piece, lightningPosition, moraleCost, durationTurns) =>
        val lightning = BoardEffect.Lightning(lightningPosition, currentTurn + durationTurns)
        this
          .changeMorale(piece.team, -moraleCost)
          .copy(boardEffects = lightning :: boardEffects)
      case TaurusRush(piece, _pieceToKill, maxDistance) =>
        val (updatedState1, pieceToKill) = guardianSwapPiece(this, piece, _pieceToKill)
        val pieceToKillPos = pieceToKill.pos
        val direction = Distance(pieceToKillPos.row - piece.pos.row, pieceToKillPos.column - piece.pos.column).toUnitVector
        val positions = BoardPos.List1to8.take(maxDistance)
          .map(distance => pieceToKillPos + direction * distance)
          .takeWhile(_.isEmpty(board))
        if (positions.lengthCompare(maxDistance) < 0) {
          // Enemy piece is destroyed, taurus stays at edge of board
          val taurusPos = if (positions.isEmpty) pieceToKill.pos else positions.last
          val (updatedState2, pieceUpdatedOption) = piece.afterMagicKill(updatedState1, pieceToKill)
          updatedState2
            .removePiece(pieceToKill)
            .updatePieceIfAlive(piece, pieceUpdatedOption.map(_.copy(pos = taurusPos)))
        } else if (positions.forall(_.isEmpty(board))) {
          // Enemy piece is moved, taurus stays one space before enemy piece
          val pieceToKillUpdated = pieceToKill.copy(pos = positions.last)
          val pieceUpdated = piece.copy(pos = positions.init.last)
          updatedState1
            .removePiece(piece)
            .removePiece(pieceToKill)
            .placePiece(pieceUpdated)
            .placePiece(pieceToKillUpdated)
        } else {
          // Enemy piece is crushed, taurus stays on the last empty space
          val taurusPos = positions.find(_.nonEmpty(board)).get - direction
          val (updatedState2, pieceUpdatedOption) = piece.afterMagicKill(updatedState1, pieceToKill)
          updatedState2
            .removePiece(pieceToKill)
            .updatePieceIfAlive(piece, pieceUpdatedOption.map(_.copy(pos = taurusPos)))
        }
      case TeleportTransformInto(piece, target, pieceData) =>
        val updatedState = piece.onTransform(this)
        val newTransformedPiece = pieceData.createPiece(target)
        updatedState
          .updatePiece(piece, newTransformedPiece)
      case MagicSummonPiece(piece, target, moraleCost, pieceData) =>
        val (updatedState, updatedPieceOption) = piece.afterMagicCast(this, None)
        updatedState
          .changeMorale(piece.team, -moraleCost)
          .placePiece(pieceData.createPiece(target))
          .updatePieceIfAlive(piece, updatedPieceOption)
      case RangedSummonGeminiTwin(piece, target, moraleCost, pieceData) =>
        val (updatedState, Some(pieceUpdated)) = piece.afterMagicCast(this, None)
        updatedState
          .changeMorale(piece.team, -moraleCost)
          .placePiece(pieceData.createPiece(target))
          .updatePiece(piece, pieceData.createPiece(pieceUpdated.pos))
      case MagicWeakEnchant(piece, pieceToWeakEnchant, durationTurns) =>
        val (pieceUpdatedOption, pieceToWeakEnchantUpdatedOption) = piece.weakEnchantPiece(this, pieceToWeakEnchant, durationTurns)
        this
          .updatePieceIfAlive(pieceToWeakEnchant, pieceToWeakEnchantUpdatedOption)
          .updatePieceIfAlive(piece, pieceUpdatedOption)
      case MagicEnvyClone(piece, _pieceToClone) =>
        val (updatedState, pieceToClone) = guardianSwapPiece(this, piece, _pieceToClone)
        val newClone = pieceToClone.data.createPiece(piece.pos)
        updatedState
          .updatePiece(piece, newClone)
      case MagicMeteor(piece, meteorPosition, moraleCost, durationTurns, pushDistance) =>
        val meteor = BoardEffect.Meteor(meteorPosition, currentTurn + durationTurns, pushDistance)
        this
          .changeMorale(piece.team, -moraleCost)
          .copy(boardEffects = meteor :: boardEffects)
      case MagicPetrify(piece, _pieceToPetrify, moraleCost, turnsPetrified) =>
        val (updatedState, pieceToPetrify) = guardianSwapPiece(this, piece, _pieceToPetrify)
        val (pieceUpdatedOption, pieceToPetrifyUpdatedOption) = piece.petrifyPiece(updatedState, pieceToPetrify, turnsPetrified)
        updatedState
          .changeMorale(piece.team, -moraleCost)
          .updatePieceIfAlive(pieceToPetrify, pieceToPetrifyUpdatedOption)
          .updatePieceIfAlive(piece, pieceUpdatedOption)
      case DummyMove(_) => this
    }

    if (turnUpdate) {
      val afterEndOfTurnPieces = newState.applyEndOfTurnActions

      val endOfTurn =
        afterEndOfTurnPieces
          .trimMorale
          .copy(movesHistory = move :: afterEndOfTurnPieces.movesHistory)
      if (endOfTurn.winner != PlayerWinType.NotFinished) {
        endOfTurn
      } else {
        val startOfNextTurn =
          endOfTurn.copy(currentTurn = endOfTurn.currentTurn + 0.5)

        val stateAfterPenalties = {
          val decayPenalty =
            if (currentTurn >= 49.5)
              startOfNextTurn.changeMorale(startOfNextTurn.getCurrentPlayer.team, -1)
            else
              startOfNextTurn

          val player = startOfNextTurn.getCurrentPlayer
          val kingMode = player.kingMode
          if (kingMode > 0)
            decayPenalty
          else if (kingMode == 0)
            decayPenalty.updatePlayer(player.copy(kingMode = -1))
          else
            decayPenalty.changeMorale(startOfNextTurn.getCurrentPlayer.team, -3)
        }

        stateAfterPenalties.startingTurnStatusEffectUpdate.trimMorale
      }
    } else {
      newState
    }
  }

  private def startingTurnStatusEffectUpdate: GameState = {

    def updatePiece(gameState: GameState, piece: Piece): (GameState, Option[Piece]) = {
      def updateEffectStatusOfPiece(
        effectsToProcess: List[EffectStatus],
        gameState: GameState,
        currentPiece: Piece
      ): (GameState, Option[Piece]) = effectsToProcess match {
        case Nil => (gameState, Some(currentPiece))
        case effect :: others =>
          def skipEffect: (GameState, Option[Piece]) =
            updateEffectStatusOfPiece(others, gameState, modify(currentPiece)(_.effectStatus).using(effect :: _))

          def removeEffect: (GameState, Option[Piece]) =
            updateEffectStatusOfPiece(others, gameState, currentPiece)

          effect match {
            case EffectStatus.Petrified(untilTurn) if untilTurn == currentTurn => removeEffect
            case EffectStatus.Frozen(untilTurn) if untilTurn == currentTurn => removeEffect
            case EffectStatus.Enchanted(untilTurn) if untilTurn == currentTurn => removeEffect
            case EffectStatus.WeakEnchanted(untilTurn) if untilTurn == currentTurn => removeEffect
            case EffectStatus.Poison(turnOfDeath) if turnOfDeath == currentTurn => (gameState, None)
            case EffectStatus.InstantKillPositional(distance) if gameState.getCurrentPlayer.team == piece.team =>
              (piece.pos + distance).getPiece(gameState.board) match {
                case Some(targetPiece) if targetPiece.team != piece.team && !targetPiece.data.isImmuneTo(EffectType.Trigger) =>
                  val playerMove =
                    if (targetPiece.canBlockFrom(piece.pos)) {
                      PlayerMove.AttackCanBeBlocked(piece, targetPiece)
                    } else {
                      PlayerMove.Attack(piece, targetPiece)
                    }

                  val gameStateUpdated1 = gameState.playPlayerMove(playerMove, turnUpdate = false)
                  val (gameStateUpdated2, pieceUpdated) = updateEffectStatusOfPiece(others, gameStateUpdated1, currentPiece)
                  (gameStateUpdated2, pieceUpdated.map(modify(_)(_.effectStatus).using(effect :: _)))
                case _ =>
                  skipEffect
              }
            case EffectStatus.PieceGrow(moraleToPromote, pieceName) if gameState.getCurrentPlayer.team != piece.team =>
              val updatedPiece = piece.changeMorale(+1)
              if (updatedPiece.currentMorale >= moraleToPromote) {
                (gameState, Some(DataLoader.getPieceData(pieceName, updatedPiece.team).createPiece(updatedPiece.pos)))
              } else {
                updateEffectStatusOfPiece(others, gameState, modify(updatedPiece)(_.effectStatus).using(effect :: _))
              }
            case _ =>
              skipEffect
          }
      }

      updateEffectStatusOfPiece(piece.effectStatus, gameState, piece.copy(effectStatus = Nil))
    }

    def updatePieces(startingState: GameState, pieces: List[Piece]): (GameState, List[Piece], List[Piece]) =
      pieces.foldLeft((startingState, List.empty[Piece], List.empty[Piece])) {
        case ((state, deadPieces, updatedPieces), piece) =>
          val (updatedGameState, maybeUpdatedPiece) = updatePiece(state, piece)
          maybeUpdatedPiece match {
            case None =>
              (updatedGameState, piece :: deadPieces, updatedPieces)
            case Some(updatedPiece) =>
              (updatedGameState, deadPieces, updatedPiece :: updatedPieces)
          }
      }

    def updatePlayer(startingState: GameState, player: Player): (GameState, Player) = {
      val (updatedState, deadPieces, updatedPieces) = updatePieces(startingState, player.piecesAffected)

      // remove all pieces that have effects
      val stateRemovedAll =
        player.piecesAffected.foldLeft(updatedState)((state, piece) => state.removePiece(piece))
      // update state with possible side effects from dead pieces
      val stateAfterDeadSideEffects =
        deadPieces.foldLeft(stateRemovedAll)((state, deadPiece) => deadPiece.afterPoisonDeath(state))
      // place all the remaining pieces back to the board
      val stateFinal =
        updatedPieces.foldLeft(stateAfterDeadSideEffects)((state, updatedPiece) => state.placePiece(updatedPiece))

      // update player object with the updated pieces (to be coherent with the stateFinal object)
      val (piecesUnaffected, piecesStillAffected) = updatedPieces.partition(_.effectStatus.isEmpty)
      val playerUpdated =
        player.copy(pieces = piecesUnaffected ++ player.pieces, piecesAffected = piecesStillAffected)

      (stateFinal, playerUpdated)
    }

    val stateAfterWhiteIsUpdated =
      if (playerWhite.piecesAffected.isEmpty)
        this
      else {
        val (updatedState, updatedPlayer) = updatePlayer(this, playerWhite)
        updatedState.copy(playerWhite = updatedPlayer)
      }
    val stateAfterBlackIsUpdated =
      if (playerBlack.piecesAffected.isEmpty)
        stateAfterWhiteIsUpdated
      else {
        val (updatedState, updatedPlayer) = updatePlayer(stateAfterWhiteIsUpdated, playerBlack)
        updatedState.copy(playerBlack = updatedPlayer)
      }

    val stateAfterBoardEffectsUpdated =
      stateAfterBlackIsUpdated.boardEffects.foldLeft(stateAfterBlackIsUpdated.copy(boardEffects = Nil)) {
        case (initialState, boardEffect) =>
          boardEffect match {
            case effect @ BoardEffect.Lightning(boardPos, turnOfLightning) =>
              if (turnOfLightning == initialState.currentTurn) {
                ???
              } else
                initialState.copy(boardEffects = effect :: initialState.boardEffects)
            case effect @ BoardEffect.Meteor(boardPos, turnOfMeteor, 1) => // TODO simplified for now
              if (turnOfMeteor == initialState.currentTurn) {
                val stateAfterPieceRemoval =
                  boardPos.getPiece(initialState.board) match {
                    case Some(piece) if !piece.data.isImmuneTo(EffectType.Magic) =>
                      initialState.removePiece(piece)
                    case _ =>
                      initialState
                  }
                Distance.adjacentDistances.map(dist => (dist, boardPos + dist))
                  .foldLeft(stateAfterPieceRemoval) { case (state, (dist, pos)) =>
                    pos.getPiece(state.board) match {
                      case Some(piece) if !piece.data.isImmuneTo(EffectType.Displacement) &&
                        (pos + dist).isEmpty(state.board) =>
                        val (updateState, pieceUpdated) = piece.moveTo(state, pos + dist)
                        updateState.updatePieceIfAlive(piece, pieceUpdated)
                      case _ =>
                        state
                    }
                  }
              } else
                initialState.copy(boardEffects = effect :: initialState.boardEffects)
          }
      }
    stateAfterBoardEffectsUpdated
  }

  def getCurrentPlayer: Player = if (currentTurn == currentTurn.toInt) playerWhite else playerBlack

  def getNextPlayer: Player = if (currentTurn == currentTurn.toInt) playerBlack else playerWhite

  def defaultValueOfState(team: PlayerTeam): Int = {
    winner match {
      case PlayerWinType.NotFinished =>
        val whitePoints = playerWhite.morale * 10 + playerWhite.numberOfPieces
        val blackPoints = playerBlack.morale * 10 + playerBlack.numberOfPieces

        team.chooseWhiteBlack(whitePoints - blackPoints, blackPoints - whitePoints)
      case PlayerWinType.PlayerWhite => if (team.isWhite) Util.ValueOfStateMaxValue else -Util.ValueOfStateMaxValue
      case PlayerWinType.PlayerBlack => if (team.isBlack) Util.ValueOfStateMaxValue else -Util.ValueOfStateMaxValue
      case PlayerWinType.Draw => -Util.ValueOfStateMaxValue / 2
    }
  }

  def addEndOfTurnPiece(pieceToAdd: Piece): GameState = {
    copy(endOfTurnActions = EndOfTurnAction.CreatePiece(pieceToAdd) :: endOfTurnActions)
  }

  def applyEndOfTurnActions: GameState = {
    endOfTurnActions.foldLeft(copy(endOfTurnActions = Nil)) {
      case (state, action) => action match {
        case EndOfTurnAction.CreatePiece(newPiece) =>
          if (newPiece.pos.isEmpty(state.board))
            state.placePiece(newPiece)
          else
            state
        case EndOfTurnAction.PieceSwapWithEnemyKing(killerPiecePos) =>
          killerPiecePos.getPiece(state.board) match {
            case None => state
            case Some(killerPiece) =>
              val enemyPlayer = state.getPlayer(killerPiece.team.enemy)
              enemyPlayer.allPieces.find(_.data.isKing) match {
                case None => state
                case Some(enemyKing) =>
                  val stateUpdated1 = state.removePiece(killerPiece).removePiece(enemyKing)
                  val (stateUpdated2, killerPieceUpdated) = killerPiece.moveTo(stateUpdated1, enemyKing.pos)
                  val (stateUpdated3, enemyKingPieceUpdated) = enemyKing.moveTo(stateUpdated2, killerPiece.pos)
                  stateUpdated3
                    .placePieceIfAlive(killerPieceUpdated)
                    .placePieceIfAlive(enemyKingPieceUpdated)
              }
          }
      }
    }
  }

}
