package ceo.play

import ceo.play.Moves.UnstoppableTeleportTransformInto
import com.softwaremill.quicklens._

case class GameState(
  board: Board,
  playerWhite: Player,
  playerBlack: Player,
  currentTurn: Double,
  movesHistory: List[PlayerMove],
  boardEffects: List[BoardEffect],
  endOfTurnActions: List[EndOfTurnAction]
) {
  def optimize: GameState = copy(
    playerWhite = playerWhite.optimizeRunners(this),
    playerBlack = playerBlack.optimizeRunners(this)
  )

  def getPlayers: List[Player] = List(playerWhite, playerBlack)

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

  def getPlayer(color: PlayerColor): Player = if (color == PlayerColor.White) playerWhite else playerBlack

  def getPlayer(team: PlayerTeam): Player = if (team.isWhite) playerWhite else playerBlack

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
    if (playerUpdated.team.isWhite)
      copy(playerWhite = playerUpdated)
    else
      copy(playerBlack = playerUpdated)
  }

  def updatePlayer(playerColor: PlayerColor, changePlayerFunction: Player => Player): GameState = {
    if (playerColor == PlayerColor.White)
      copy(playerWhite = changePlayerFunction(playerWhite))
    else
      copy(playerBlack = changePlayerFunction(playerBlack))
  }

  /**
    * Places piece and updates morale/player pieces
    */
  def placePiece(piece: Piece): GameState = {
    val withNewPiece = copy(board = board.place(piece))

    if (piece.team.isWhite) {
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

    if (piece.team.isWhite) {
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
    if (playerTeam.isWhite)
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

    allMoves
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

  private def guardianSwapPiece(attackerPiece: Piece, piece: Piece): (GameState, Piece) = {
    if (attackerPiece.team != piece.team && piece.data.isChampion && !piece.data.isImmuneTo(EffectType.Displacement)) {
      getPlayer(piece.team).extraData.guardedPositions.get(piece.pos) match {
        case Some(guardianPiece) =>
          val updatedState = playPlayerMove(PlayerMove.Swap(piece, guardianPiece), turnUpdate = false)
          (updatedState, piece.pos.getPiece(updatedState.board).get)
        case None =>
          (this, piece)
      }
    } else {
      (this, piece)
    }
  }

  def playPlayerMove(move: PlayerMove, turnUpdate: Boolean): GameState = {
    import PlayerMove._

    val newState: GameState = move match {
      // Standard moves:
      case Move(piece, target) =>
        val (updatedState, pieceNewPos) = piece.moveTo(this, target, isFromPlayerMove = true)
        updatedState
          .updatePieceIfAlive(piece, pieceNewPos)
      case Attack(piece, _pieceToKill) =>
        val (updatedState1, pieceToKill) = guardianSwapPiece(piece, _pieceToKill)
        val (updatedState2, pieceNewPosOption) = piece.afterMeleeKill(updatedState1, pieceToKill)
        updatedState2
          .removePiece(pieceToKill)
          .updatePieceIfAlive(piece, pieceNewPosOption)
      case AttackCanBeBlocked(piece, _pieceToAttack) =>
        val (updatedState, pieceToAttack) = guardianSwapPiece(piece, _pieceToAttack)
        if (pieceToAttack eq _pieceToAttack) {
          val pieceToKillUpdated = pieceToAttack.removeBlockEffect
          updatedState.updatePiece(pieceToAttack, pieceToKillUpdated)
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
        val (updatedState1, pieceToDestroy) = guardianSwapPiece(piece, _pieceToDestroy)
        val (updatedState2, pieceUpdatedOption) = piece.afterMagicKill(updatedState1, pieceToDestroy)
        updatedState2
          .removePiece(pieceToDestroy)
          .updatePieceIfAlive(piece, pieceUpdatedOption)
      case RangedPetrify(piece, _pieceToPetrify, turnsPetrified) =>
        val (updatedState, pieceToPetrify) = guardianSwapPiece(piece, _pieceToPetrify)
        val (pieceUpdatedOption, pieceToPetrifyUpdatedOption) = piece.petrifyPiece(updatedState, pieceToPetrify, turnsPetrified)
        updatedState
          .updatePieceIfAlive(pieceToPetrify, pieceToPetrifyUpdatedOption)
          .updatePieceIfAlive(piece, pieceUpdatedOption)
      case RangedPush(piece, _pieceToPush, moraleCost, maxPushDistance) =>
        val (updatedState1, pieceToPush) = guardianSwapPiece(piece, _pieceToPush)
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
        val (updatedState1, pieceToDestroy) = guardianSwapPiece(piece, _pieceToDestroy)
        val (updatedState2, pieceUpdatedOption) = piece.afterMagicKill(updatedState1, pieceToDestroy)
        updatedState2
          .removePiece(pieceToDestroy)
          .updatePieceIfAlive(piece, pieceUpdatedOption)
      case MagicPoison(piece, _pieceToPoison, turnsToDeath) =>
        val (updatedState, pieceToPoison) = guardianSwapPiece(piece, _pieceToPoison)
        val (updatedPieceOption, pieceToPoisonUpdatedOption) = piece.poisonPiece(updatedState, pieceToPoison, turnsToDeath)
        updatedState
          .updatePieceIfAlive(pieceToPoison, pieceToPoisonUpdatedOption)
          .updatePieceIfAlive(piece, updatedPieceOption)
      case MagicCharm(piece, _pieceToCharm) =>
        val (updatedState1, pieceToCharm) = guardianSwapPiece(piece, _pieceToCharm)
        val pieceToCharmUpdated = pieceToCharm.swapTeams
        val (updatedState2, pieceUpdated) = piece.afterMagicCast(updatedState1, Some(pieceToCharmUpdated))
        updatedState2
          .updatePieceIfAlive(piece, pieceUpdated)
          .updatePiece(pieceToCharm, pieceToCharmUpdated)
      case TransformIntoAllyPiece(piece, _pieceToTransform, moraleCost, allyPieceData) =>
        val (updatedState1, pieceToTransform) = guardianSwapPiece(piece, _pieceToTransform)
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
        val (updatedState1, pieceToTeleport) = guardianSwapPiece(piece, _pieceToTeleport)
        val (updatedState2, pieceToTeleportUpdated) = pieceToTeleport.moveTo(updatedState1, target)
        updatedState2
          .updatePieceIfAlive(pieceToTeleport, pieceToTeleportUpdated)
      case MagicPush(piece, _pieceToPush, moraleCost, maxPushDistance) =>
        val (updatedState1, pieceToPush) = guardianSwapPiece(piece, _pieceToPush)
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
        val (updatedState, pieceToFreeze) = guardianSwapPiece(piece, _pieceToFreeze)
        val (pieceUpdatedOption, pieceToFreezeUpdatedOption) = piece.freezePiece(updatedState, pieceToFreeze, freezeDuration)
        updatedState
          .updatePieceIfAlive(pieceToFreeze, pieceToFreezeUpdatedOption)
          .updatePieceIfAlive(piece, pieceUpdatedOption)
      case MagicPushFreeze(piece, _pieceToPushFreeze, maxPushDistance, freezeDuration) =>
        val (updatedState, pieceToPushFreeze) = guardianSwapPiece(piece, _pieceToPushFreeze)
        val (pieceUpdatedOption, pieceToPushFreezeUpdatedOption) = piece.freezePiece(updatedState, pieceToPushFreeze, freezeDuration)
        updatedState
          .updatePieceIfAlive(pieceToPushFreeze, pieceToPushFreezeUpdatedOption)
          .updatePieceIfAlive(piece, pieceUpdatedOption)
          .doActionIfCondition(pieceUpdatedOption.isDefined && pieceToPushFreezeUpdatedOption.isDefined,
            _.playPlayerMove(PlayerMove.MagicPush(pieceUpdatedOption.get, pieceToPushFreezeUpdatedOption.get, 0, maxPushDistance), turnUpdate = false))
      case MagicLightning(piece, lightningPosition, moraleCost, durationTurns) =>
        val lightning = BoardEffect.Lightning(lightningPosition, currentTurn + durationTurns)
        this
          .changeMorale(piece.team, -moraleCost)
          .copy(boardEffects = lightning :: boardEffects)
      case TaurusRush(piece, _pieceToKill, maxDistance) =>
        val (updatedState1, pieceToKill) = guardianSwapPiece(piece, _pieceToKill)
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
        val (updatedState, pieceToClone) = guardianSwapPiece(piece, _pieceToClone)
        val newClone = pieceToClone.swapTeams.data.createPiece(piece.pos)
        updatedState
          .updatePiece(piece, newClone)
      case MagicMeteor(piece, meteorPosition, moraleCost, durationTurns) =>
        val meteor = BoardEffect.Meteor(meteorPosition, currentTurn + durationTurns)
        this
          .changeMorale(piece.team, -moraleCost)
          .copy(boardEffects = meteor :: boardEffects)
      case MagicPetrify(piece, _pieceToPetrify, moraleCost, turnsPetrified) =>
        val (updatedState, pieceToPetrify) = guardianSwapPiece(piece, _pieceToPetrify)
        val (pieceUpdatedOption, pieceToPetrifyUpdatedOption) = piece.petrifyPiece(updatedState, pieceToPetrify, turnsPetrified)
        updatedState
          .changeMorale(piece.team, -moraleCost)
          .updatePieceIfAlive(pieceToPetrify, pieceToPetrifyUpdatedOption)
          .updatePieceIfAlive(piece, pieceUpdatedOption)
      case RangedCompel(piece, _pieceToCompel, turnsCompelled) =>
        val (updatedState, pieceToCompel) = guardianSwapPiece(piece, _pieceToCompel)
        val (pieceUpdated, pieceToCompelUpdated) = piece.compelPiece(updatedState, pieceToCompel, turnsCompelled)
        updatedState
          .updatePieceIfAlive(piece, pieceUpdated)
          .updatePieceIfAlive(pieceToCompel, pieceToCompelUpdated)
      case MagicPushTowards(piece, _pieceToPush, moraleCost, maxPushDistance) =>
        val (updatedState1, pieceToPush) = guardianSwapPiece(piece, _pieceToPush)
        val pieceToPushPos = pieceToPush.pos
        val targetFinalPosition = {
          val max = Math.max(Math.abs(maxPushDistance.rowDiff), Math.abs(maxPushDistance.columnDiff))
          val dir = maxPushDistance.toUnitVector
          val positions = BoardPos.List1to8.view(0, max)
            .map(distance => pieceToPushPos + dir * distance)
            .takeWhile(_.isEmpty(board))
          positions.last
        }

        val (updatedState2, pieceToPushUpdated) = pieceToPush.moveTo(updatedState1, targetFinalPosition)
        updatedState2
          .changeMorale(piece.team, -moraleCost)
          .updatePieceIfAlive(pieceToPush, pieceToPushUpdated)
      case MagicSuicideFreeze(piece, _pieceToFreeze, freezeDuration) =>
        // TODO Is it possible to trigger guardian here ?
        val (updatedState1, pieceToFreeze) = guardianSwapPiece(piece, _pieceToFreeze)

        // TODO here is a good use of the "static" freezePiece using None (because the Comet is already death on magic freeze ...)
        val (_, pieceToFreezeUpdatedOption) = piece.freezePiece(updatedState1, pieceToFreeze, freezeDuration)
        val updatedState2 =
          updatedState1
            .updatePieceIfAlive(pieceToFreeze, pieceToFreezeUpdatedOption)
            .removePiece(piece)

        // Simulate Comet dying on the selected target
        piece.copy(pos = pieceToFreeze.pos).onTransform(updatedState2)
      case MagicDestroySelfButterfly(piece, target, turnsDelay, turnsEnchanted) =>
        val butterflyEffect = BoardEffect.Butterfly(target, currentTurn + turnsDelay, turnsEnchanted, piece.data)
        this
          .removePiece(piece)
          .copy(boardEffects = butterflyEffect :: boardEffects)
      case DummyMove(_) => this
    }

    if (turnUpdate) {
      val stateAfterEndOfTurn = newState.endOfTurn

      val endOfTurn =
        stateAfterEndOfTurn
          .trimMorale
          .copy(movesHistory = move :: stateAfterEndOfTurn.movesHistory)
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

    def updatePieceStatusEffects(gameState: GameState, piece: Piece): (GameState, Option[Piece]) = {
      val playerTeam = gameState.getCurrentPlayer.team

      def updateEffectStatusOfPiece(
        effectsToProcess: List[EffectStatus],
        gameState: GameState,
        currentPiece: Piece
      ): (GameState, Option[Piece]) = effectsToProcess match {
        case Nil => (gameState, Some(currentPiece))
        case effect :: others =>
          def skipEffect(piece: Piece = currentPiece): (GameState, Option[Piece]) =
            updateEffectStatusOfPiece(others, gameState, modify(piece)(_.effectStatus).using(effect :: _))

          def removeEffect(piece: Piece = currentPiece): (GameState, Option[Piece]) =
            updateEffectStatusOfPiece(others, gameState, piece)

          effect match {
            case EffectStatus.Petrified(untilTurn) if untilTurn == currentTurn => removeEffect()
            case EffectStatus.Frozen(untilTurn) if untilTurn == currentTurn => removeEffect()
            case EffectStatus.Enchanted(untilTurn) if untilTurn == currentTurn => removeEffect()
            case EffectStatus.WeakEnchanted(untilTurn) if untilTurn == currentTurn => removeEffect()
            case EffectStatus.Poison(turnOfDeath) if turnOfDeath == currentTurn => (gameState, None)
            case EffectStatus.PieceGrow(moraleToPromote, pieceName) if playerTeam != piece.team =>
              val updatedPiece = piece.changeMorale(+1)
              if (updatedPiece.currentMorale >= moraleToPromote) {
                (gameState, Some(DataLoader.getPieceData(pieceName, updatedPiece.team).createPiece(updatedPiece.pos)))
              } else {
                skipEffect(updatedPiece)
              }
            case EffectStatus.Compel(untilTurn, _) if untilTurn == currentTurn => removeEffect()
            case EffectStatus.Compel(_, distanceToMove) if playerTeam == piece.team =>
              val target = currentPiece.pos + distanceToMove
              if (target.isEmpty(gameState.board)) {
                val (gameStateUpdated, pieceUpdatedOption) = currentPiece.moveTo(gameState, target)
                pieceUpdatedOption match {
                  case None =>
                    (gameStateUpdated, None)
                  case Some(pieceUpdated) =>
                    updateEffectStatusOfPiece(others, gameStateUpdated, modify(pieceUpdated)(_.effectStatus).using(effect :: _))
                }
              } else {
                skipEffect()
              }
            case EffectStatus.DecayAfterTurn(turnStarts, moralePerTurn) if currentTurn >= turnStarts && playerTeam == piece.team =>
              val newMorale = currentPiece.currentMorale - moralePerTurn
              if (newMorale > 0)
                skipEffect(currentPiece.setMorale(newMorale))
              else
                (gameState, None)
            case _ =>
              skipEffect()
          }
      }

      updateEffectStatusOfPiece(piece.effectStatus, gameState, piece.copy(effectStatus = Nil))
    }

    def updatePieceTriggers(gameState: GameState, piece: Piece): (GameState, Option[Piece]) = {
      val player = gameState.getCurrentPlayer
      val playerTeam = player.team

      if (piece.data.isSamurai && playerTeam == piece.team && piece.canAct(player)) {
        piece.data.powers.collectFirst { case power: Powers.TriggerInstantKill => power } match {
          case Some(Powers.TriggerInstantKill(distance)) =>
            val target = piece.pos + distance
            target.getPiece(gameState.board) match {
              case Some(targetPiece) if {
                targetPiece.team != playerTeam &&
                  !targetPiece.isEnchanted &&
                  !targetPiece.data.isImmuneTo(EffectType.Trigger) &&
                  Moves.generalCanTargetEnemy(piece, targetPiece)
              } =>
                if (targetPiece.canBlockFrom(piece.pos)) {
                  val (updatedState, pieceToAttack) = gameState.guardianSwapPiece(piece, targetPiece)
                  if (pieceToAttack eq targetPiece) {
                    val pieceToKillUpdated = pieceToAttack.removeBlockEffect
                    (updatedState.updatePiece(pieceToAttack, pieceToKillUpdated), Some(piece))
                  } else {
                    val (updatedState1, pieceToKill) = gameState.guardianSwapPiece(piece, targetPiece)
                    val (updatedState2, pieceNewPosOption) = piece.afterMeleeKill(updatedState1, pieceToKill)
                    (updatedState2.removePiece(pieceToKill), pieceNewPosOption)
                  }
                } else {
                  val (updatedState1, pieceToKill) = gameState.guardianSwapPiece(piece, targetPiece)
                  val (updatedState2, pieceNewPosOption) = piece.afterMeleeKill(updatedState1, pieceToKill)
                  (updatedState2.removePiece(pieceToKill), pieceNewPosOption)
                }
              case _ =>
                (gameState, Some(piece))
            }
          case _ =>
            (gameState, Some(piece))
        }
      } else {
        (gameState, Some(piece))
      }
    }

    def updatePieces(startingState: GameState, pieces: List[Piece]): (GameState, List[Piece], List[Piece]) =
      pieces.foldLeft((startingState, List.empty[Piece], List.empty[Piece])) {
        case ((state, deadPieces, updatedPieces), piece) =>
          val (updatedGameState1, maybeUpdatedPiece1) = updatePieceStatusEffects(state, piece)
          maybeUpdatedPiece1 match {
            case None =>
              (updatedGameState1, piece :: deadPieces, updatedPieces)
            case Some(updatedPiece) =>
              val (updatedGameState2, maybeUpdatedPiece2) = updatePieceTriggers(updatedGameState1, updatedPiece)
              maybeUpdatedPiece2 match {
                case None =>
                  (updatedGameState2, piece :: deadPieces, updatedPieces)
                case Some(updatedPiece2) =>
                  (updatedGameState2, deadPieces, updatedPiece2 :: updatedPieces)
              }
          }
      }

    def updatePlayer(startingState: GameState, player: Player): GameState = {
      val (updatedState, deadPieces, updatedPieces) = updatePieces(startingState, player.piecesAffected)

      // remove all pieces that have effects
      val stateRemovedAll =
        player.piecesAffected.foldLeft(updatedState)((state, piece) => state.removePiece(piece))
      // update state with possible side effects from dead pieces
      val stateAfterDeadSideEffects =
        deadPieces.foldLeft(stateRemovedAll)((state, deadPiece) => deadPiece.afterPoisonDeath(state))
      // place all the remaining pieces back to the board
      updatedPieces.foldLeft(stateAfterDeadSideEffects)((state, updatedPiece) => state.placePiece(updatedPiece))
    }

    val stateAfterWhiteIsUpdated =
      if (playerWhite.piecesAffected.isEmpty)
        this
      else
        updatePlayer(this, playerWhite)
    val stateAfterBlackIsUpdated =
      if (playerBlack.piecesAffected.isEmpty)
        stateAfterWhiteIsUpdated
      else
        updatePlayer(stateAfterWhiteIsUpdated, stateAfterWhiteIsUpdated.getPlayer(PlayerColor.Black))

    val stateAfterBoardEffectsUpdated =
      stateAfterBlackIsUpdated.boardEffects.foldLeft(stateAfterBlackIsUpdated.copy(boardEffects = Nil)) {
        case (initialState, boardEffect) =>
          boardEffect match {
            case effect @ BoardEffect.Lightning(boardPos, turnOfLightning) =>
              if (turnOfLightning == initialState.currentTurn) {
                boardPos.getPiece(initialState.board) match {
                  case Some(piece) if !piece.data.isImmuneTo(EffectType.Magic) =>
                    initialState.removePiece(piece)
                  case _ =>
                    initialState
                }
              } else
                initialState.copy(boardEffects = effect :: initialState.boardEffects)
            case effect @ BoardEffect.Meteor(boardPos, turnOfMeteor) =>
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
                      case Some(piece) if {
                        !piece.data.isImmuneTo(EffectType.Magic) &&
                          !piece.data.isImmuneTo(EffectType.Displacement) &&
                          (pos + dist).isEmpty(state.board)
                      } =>
                        val (updateState, pieceUpdated) = piece.moveTo(state, pos + dist)
                        updateState.updatePieceIfAlive(piece, pieceUpdated)
                      case _ =>
                        state
                    }
                  }
              } else
                initialState.copy(boardEffects = effect :: initialState.boardEffects)
            case effect @ BoardEffect.Butterfly(boardPos, turnOfEffect, enchantDuration, pieceDataOnRevive) =>
              if (turnOfEffect == initialState.currentTurn) {
                boardPos.getPiece(initialState.board) match {
                  case Some(piece) if !piece.data.isImmuneTo(EffectType.Magic) =>
                    if (piece.team == pieceDataOnRevive.team) {
                      val (_, pieceUpdated) = piece.enchantPiece(initialState, piece, enchantDuration)
                      initialState.updatePieceIfAlive(piece, pieceUpdated)
                    } else {
                      initialState.removePiece(piece)
                    }
                  case None =>
                    initialState.placePiece(pieceDataOnRevive.createPiece(boardPos))
                  case _ =>
                    initialState
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

  def addEndOfTurnAction(action: EndOfTurnAction): GameState = copy(endOfTurnActions = action :: endOfTurnActions)

  def addEndOfTurnPiece(pieceToAdd: Piece): GameState = {
    addEndOfTurnAction(EndOfTurnAction.CreatePiece(pieceToAdd))
  }

  def endOfTurn: GameState = {
    val stateAfterEndOfTurnActions = applyEndOfTurnActions
    val nextPlayer = getNextPlayer
    if (!nextPlayer.extraData.hasEndOfTurnTriggers) {
      stateAfterEndOfTurnActions
    } else {
      val team = nextPlayer.team
      nextPlayer.allPieces.foldLeft(stateAfterEndOfTurnActions) {
        case (state, piece) =>
          piece.data.powers.collectFirst {
            case Powers.MagicTriggerLust(distances) => distances
          } match {
            case None => state
            case Some(distances) =>
              val pos = piece.pos
              distances.foldLeft(state) {
                (state, dist) =>
                  (pos + dist).getPiece(state.board) match {
                    case Some(targetPiece) if {
                      targetPiece.team != team &&
                        !targetPiece.data.isImmuneTo(EffectType.Magic) &&
                        !targetPiece.data.isImmuneTo(EffectType.Trigger) &&
                        !targetPiece.data.isImmuneTo(EffectType.Displacement)
                    } =>
                      val targetPos = targetPiece.pos + (pos - targetPiece.pos).toUnitVector
                      if (targetPos.isEmpty(state.board))
                        state.removePiece(targetPiece).addEndOfTurnPiece(targetPiece.copy(pos = targetPos))
                      else
                        state
                    case _ => state
                  }
              }
          }
      }.applyEndOfTurnActions
    }
  }

  def applyEndOfTurnActions: GameState = {
    endOfTurnActions.foldLeft(copy(endOfTurnActions = Nil)) {
      case (initialState, action) =>
        action match {
          case EndOfTurnAction.CreatePiece(newPiece) =>
            if (newPiece.pos.isEmpty(initialState.board))
              initialState.placePiece(newPiece)
            else
              initialState
          case EndOfTurnAction.PieceSwapWithEnemyKing(killerPiecePos) =>
            killerPiecePos.getPiece(initialState.board) match {
              case None => initialState
              case Some(killerPiece) =>
                val enemyPlayer = initialState.getPlayer(killerPiece.team.enemy)
                enemyPlayer.allPieces.find(_.data.isKing) match {
                  case None => initialState
                  case Some(enemyKing) =>
                    val stateUpdated1 = initialState.removePiece(killerPiece).removePiece(enemyKing)
                    val (stateUpdated2, killerPieceUpdated) = killerPiece.moveTo(stateUpdated1, enemyKing.pos)
                    val (stateUpdated3, enemyKingPieceUpdated) = enemyKing.moveTo(stateUpdated2, killerPiece.pos)
                    stateUpdated3
                      .placePieceIfAlive(killerPieceUpdated)
                      .placePieceIfAlive(enemyKingPieceUpdated)
                }
            }
          case EndOfTurnAction.MoveDoves(playerTeam) =>
            /*
            Here we have to simulate a v0.52 bug:
            Dove get updated from square (0,0) to (7,7), left to right, top to bottom, if they are blocked by other doves
            They will not be updated (this code has to simulate that bug to be "correct"):
             */

            val player = initialState.getPlayer(playerTeam)

            val directionForward = player.directionForward
            player.allPieces
              .filter(_.data.isDove)
              .sortBy(_.pos)(BoardPos.byScanOrder)
              .foldLeft(initialState) {
                (state, dovePiece) =>
                  val positionForward = dovePiece.pos + directionForward
                  if (positionForward.isEmpty(state.board))
                    state.updatePiece(dovePiece, dovePiece.copy(pos = positionForward))
                  else
                    state
              }
          case EndOfTurnAction.AquariusExploded(deadAquariusPiece, pushDistance, freezeDuration) =>
            Distance.adjacentDistances.map(_ + deadAquariusPiece.pos).foldLeft(initialState) { (gameState, boardPos) =>
              Moves.canMagicPushFreeze(deadAquariusPiece, boardPos, pushDistance, freezeDuration, gameState) match {
                case None => gameState
                case Some(playerMove) =>
                  gameState.playPlayerMove(playerMove, turnUpdate = false)
              }
            }
        }
    }
  }

  def getReplayInfo: String = {
    val sb = new StringBuilder()

    def appendLine(line: String) = sb.append(line + "\n")

    def appendSmallSeparator(): Unit = appendLine("-" * 3)

    def appendTurnSeparator(): Unit = appendLine("-" * 20)

    movesHistory.headOption.foreach { lastMove =>
      appendLine(lastMove.toString)
      appendSmallSeparator()
    }

    appendLine("Turn " + currentTurn + " " + getCurrentPlayer.team)
    appendSmallSeparator()

    appendLine(board.getReplayInfo)
    appendSmallSeparator()

    sb.append(playerWhite.getReplayInfo)
    appendSmallSeparator()

    sb.append(playerBlack.getReplayInfo)
    appendSmallSeparator()

    appendLine(boardEffects.map(_.getReplayInfo).sorted.mkString(","))
    appendTurnSeparator()

    sb.toString()
  }

}
