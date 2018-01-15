package ceo.play

import ceo.play.Moves.UnstoppableTeleportTransformInto
import ceo.play.PlayerTeam.{Black, White}

case class GameState(
  board: Board,
  playerWhite: Player,
  playerBlack: Player,
  currentTurn: Double,
  movesHistory: List[PlayerMove],
  boardEffects: List[BoardEffect],
  gameRunner: GameRunner
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

    def guardianSwapPiece(state: GameState, piece: Piece): (GameState, Piece) = {
      if (piece.data.isChampion && !piece.data.isImmuneTo(EffectType.Displacement)) {
        state.getPlayer(piece.team).extraData.guardedPositions.get(piece.pos) match {
          case Some(guardianPiece) =>
            val updatedState = playPlayerMove(PlayerMove.Swap(piece, guardianPiece), turnUpdate = false)
            (updatedState, piece.pos.getPiece(updatedState.board).get)
          case None => (state, piece)
        }
      } else {
        (state, piece)
      }
    }

    val newState: GameState = move match {
      // Standard moves:
      case Move(piece, target) =>
        val (updatedState, pieceNewPos) = piece.moveTo(this, target)
        updatedState
          .updatePiece(piece, pieceNewPos)
      case Attack(piece, _pieceToKill) =>
        val (updatedState1, pieceToKill) = guardianSwapPiece(this, _pieceToKill)
        val (updatedState2, pieceNewPosOpt) = piece.afterMeleeKill(updatedState1, pieceToKill)
        val updatedStateAfterRemoves =
          updatedState2
            .removePiece(pieceToKill)
            .removePiece(piece)
        pieceNewPosOpt match {
          case Some(pieceNewPos) => updatedStateAfterRemoves.placePiece(pieceNewPos)
          case _ => updatedStateAfterRemoves
        }
      case AttackCanBeBlocked(piece, _pieceToAttack) =>
        val (updatedState, pieceToAttack) = guardianSwapPiece(this, _pieceToAttack)
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
          .placePiece(pieceUpdated)
          .placePiece(pieceToSwapUpdated)
      // Ranged moves:
      case RangedDestroy(piece, _pieceToDestroy) =>
        val (updatedState1, pieceToDestroy) = guardianSwapPiece(this, _pieceToDestroy)
        val (updatedState2, pieceUpdatedOpt) = piece.afterMagicKill(updatedState1, pieceToDestroy)
        val updatedStateAfterRemoves =
          updatedState2
            .removePiece(pieceToDestroy)
            .removePiece(piece)
        pieceUpdatedOpt match {
          case Some(pieceUpdated) => updatedStateAfterRemoves.placePiece(pieceUpdated)
          case None => updatedStateAfterRemoves
        }
      case RangedPetrify(_, _pieceToPetrify, turnsPetrified) =>
        val (updatedState, pieceToPetrify) = guardianSwapPiece(this, _pieceToPetrify)
        val pieceToPetrifyUpdated = pieceToPetrify.petrify(updatedState, turnsPetrified)
        updatedState
          .updatePiece(pieceToPetrify, pieceToPetrifyUpdated)
      case RangedPush(piece, _pieceToPush, moraleCost, maxPushDistance) =>
        val (updatedState1, pieceToPush) = guardianSwapPiece(this, _pieceToPush)
        val pieceToPushPos = pieceToPush.pos
        val dir = (pieceToPushPos - piece.pos).toUnitVector
        val positions = BoardPos.List1to8.view(0, maxPushDistance)
          .map(distance => pieceToPushPos + dir * distance)
          .takeWhile(_.isEmpty(board))

        val (updatedState2, pieceToPushUpdated) = pieceToPush.moveTo(updatedState1, positions.last)
        updatedState2
          .changeMorale(piece.team, -moraleCost)
          .updatePiece(pieceToPush, pieceToPushUpdated)
      // Magic moves:
      case MagicDestroy(piece, _pieceToDestroy) =>
        val (updatedState1, pieceToDestroy) = guardianSwapPiece(this, _pieceToDestroy)
        val (updatedState2, pieceUpdatedOpt) = piece.afterMagicKill(updatedState1, pieceToDestroy)
        val updatedStateAfterRemoves =
          updatedState2
            .removePiece(pieceToDestroy)
            .removePiece(piece)
        pieceUpdatedOpt match {
          case Some(pieceUpdated) => updatedStateAfterRemoves.placePiece(pieceUpdated)
          case None => updatedStateAfterRemoves
        }
      case MagicPoison(piece, _pieceToPoison, turnsToDeath) =>
        val (updatedState1, pieceToPoison) = guardianSwapPiece(this, _pieceToPoison)
        val (updatedState2, updatedPiece, updatedPoisonedPiece) = piece.afterPoisonPiece(pieceToPoison, turnsToDeath, updatedState1)
        updatedState2
          .updatePiece(piece, updatedPiece)
          .updatePiece(pieceToPoison, updatedPoisonedPiece)
      case MagicCharm(_, _pieceToCharm) =>
        val (updatedState, pieceToCharm) = guardianSwapPiece(this, _pieceToCharm)
        val pieceToCharmUpdated = pieceToCharm.swapTeams
        updatedState
          .updatePiece(pieceToCharm, pieceToCharmUpdated)
      case TransformEnemyIntoAllyPiece(piece, _pieceToTransform, moraleCost, allyPieceData) =>
        val (updatedState1, pieceToTransform) = guardianSwapPiece(this, _pieceToTransform)
        val (updatedState2, pieceUpdatedOpt) = piece.afterMagicKill(updatedState1, pieceToTransform)
        val newTransformedPiece = allyPieceData.createPiece(pieceToTransform.pos)
        val updatedState3 =
          updatedState2
            .changeMorale(piece.team, -moraleCost)
            .updatePiece(pieceToTransform, newTransformedPiece)
        pieceUpdatedOpt match {
          case Some(pieceUpdated) => updatedState3.placePiece(pieceUpdated)
          case None => updatedState3
        }
      case KingDoesCastling(kingPiece, allyPiece, kingTarget, allyTarget) =>
        val kingWithoutCastling = DataLoader
          .getPieceData("King-no-Cas", kingPiece.team) // TODO make this a StatusEffect instead of a new piece
          .createPiece(kingTarget)
          .setMorale(kingPiece.currentMorale)
          .addAllEffects(kingPiece.effectStatus)
        this
          .updatePiece(allyPiece, allyPiece.copy(pos = allyTarget))
          .updatePiece(kingPiece, kingWithoutCastling)
      case TeleportPiece(_, _pieceToTeleport, target, _, _) =>
        val (updatedState1, pieceToTeleport) = guardianSwapPiece(this, _pieceToTeleport)
        val (updatedState2, pieceToTeleportUpdated) = pieceToTeleport.moveTo(updatedState1, target)
        updatedState2
          .updatePiece(pieceToTeleport, pieceToTeleportUpdated)
      case MagicPush(piece, _pieceToPush, maxPushDistance) =>
        val (updatedState1, pieceToPush) = guardianSwapPiece(this, _pieceToPush)
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
          .updatePiece(pieceToPush, pieceToPushUpdated)
      case MagicFreeze(_, _pieceToFreeze, freezeDuration) =>
        val (updatedState, pieceToFreeze) = guardianSwapPiece(this, _pieceToFreeze)
        val pieceToFreezeUpdated = pieceToFreeze.freeze(updatedState, freezeDuration)
        updatedState
          .updatePiece(pieceToFreeze, pieceToFreezeUpdated)
      case MagicPushFreeze(piece, _pieceToPushFreeze, maxPushDistance, freezeDuration) =>
        val (updatedState1, pieceToPushFreeze) = guardianSwapPiece(this, _pieceToPushFreeze)
        val pieceToPushFreezeUpdated = pieceToPushFreeze.freeze(updatedState1, freezeDuration)
        val updatedState2 =
          updatedState1
            .updatePiece(pieceToPushFreeze, pieceToPushFreezeUpdated)
        updatedState2.playPlayerMove(PlayerMove.MagicPush(piece, pieceToPushFreezeUpdated, maxPushDistance), turnUpdate = false)
      case MagicLightning(piece, lightningPosition, moraleCost, durationTurns) =>
        val lightning = BoardEffect.Lightning(lightningPosition, currentTurn + durationTurns)
        this
          .changeMorale(piece.team, -moraleCost)
          .copy(boardEffects = lightning :: boardEffects)
      case TaurusRush(piece, _pieceToKill, maxDistance) =>
        val (updatedState1, pieceToKill) = guardianSwapPiece(this, _pieceToKill)
        val pieceToKillPos = pieceToKill.pos
        val direction = Distance(pieceToKillPos.row - piece.pos.row, pieceToKillPos.column - piece.pos.column).toUnitVector
        val positions = BoardPos.List1to8.take(maxDistance)
          .map(distance => pieceToKillPos + direction * distance)
          .takeWhile(_.isEmpty(board))
        if (positions.lengthCompare(maxDistance) < 0) {
          // Enemy piece is destroyed, taurus stays at edge of board
          val taurusPos = if (positions.isEmpty) pieceToKill.pos else positions.last
          val (updatedState2, pieceUpdatedOpt) = piece.afterMagicKill(updatedState1, pieceToKill)
          val updatedState3 =
            updatedState2
              .removePiece(piece)
              .removePiece(pieceToKill)
          pieceUpdatedOpt match {
            case Some(pieceUpdated) => updatedState3.placePiece(pieceUpdated.copy(pos = taurusPos))
            case None => updatedState3
          }
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
          val (updatedState2, pieceUpdatedOpt) = piece.afterMagicKill(updatedState1, pieceToKill)
          val updatedState3 =
            updatedState2
              .removePiece(piece)
              .removePiece(pieceToKill)
          pieceUpdatedOpt match {
            case Some(pieceUpdated) => updatedState3.placePiece(pieceUpdated.copy(pos = taurusPos))
            case None => updatedState3
          }
        }
      case TeleportTransformInto(piece, target, pieceData) =>
        val updatedState = piece.onSuicide(this)
        val newTransformedPiece = pieceData.createPiece(target)
        updatedState
          .updatePiece(piece, newTransformedPiece)
      case MagicCreatePiece(piece, target, moraleCost, pieceData) =>
        val (updatedState1, maybeUpdatedPiece) = piece.afterMagicCast(this)
        val updatedState2 =
          updatedState1
            .changeMorale(piece.team, -moraleCost)
            .placePiece(pieceData.createPiece(target))
            .removePiece(piece)
        maybeUpdatedPiece match {
          case Some(updatedPiece) =>
            updatedState2.placePiece(updatedPiece)
          case None =>
            updatedState2
        }
      case RangedSummonGeminiTwin(piece, target, moraleCost, pieceData) =>
        val (updatedState1, maybeUpdatedPiece) = piece.afterMagicCast(this)
        val updatedState2 =
          updatedState1
            .changeMorale(piece.team, -moraleCost)
            .placePiece(pieceData.createPiece(target))
            .removePiece(piece)
        maybeUpdatedPiece match {
          case Some(_) =>
            updatedState2.placePiece(pieceData.createPiece(piece.pos))
          case None =>
            updatedState2
        }
      case MagicWeakEnchant(piece, pieceToWeakEnchant, durationTurns) =>
        val (updatedState1, maybeUpdatedPiece) = piece.afterMagicCast(this)
        val pieceToWeakEnchantUpdated = pieceToWeakEnchant.weakEnchant(updatedState1, durationTurns)
        maybeUpdatedPiece match {
          case Some(pieceUpdated) if pieceUpdated == piece =>
            updatedState1
              .updatePiece(pieceToWeakEnchant, pieceToWeakEnchantUpdated)
          case Some(pieceUpdated) =>
            updatedState1
              .updatePiece(piece, pieceUpdated)
              .updatePiece(pieceToWeakEnchant, pieceToWeakEnchantUpdated)
          case None =>
            updatedState1
        }
      case MagicEnvyClone(piece, _pieceToClone) =>
        val (updatedState, pieceToClone) = guardianSwapPiece(this, _pieceToClone)
        val newClone = pieceToClone.data.createPiece(piece.pos)
        updatedState
          .updatePiece(piece, newClone)
      case DummyMove(_) => this
    }

    if (turnUpdate) {
      val stateWithPenalties = {
        val decayPenalty =
          if (currentTurn >= 50)
            newState.changeMorale(getNextPlayer.team, -1)
          else
            newState
        if (getNextPlayer.hasKing)
          decayPenalty
        else
          decayPenalty.changeMorale(getNextPlayer.team, -3)
      }
      stateWithPenalties.trimMorale.copy(
        currentTurn = stateWithPenalties.currentTurn + 0.5,
        movesHistory = move :: stateWithPenalties.movesHistory
      ).startingTurnStatusEffectUpdate
    } else {
      newState
    }
  }

  private def startingTurnStatusEffectUpdate: GameState = {

    def updatePiece(gameState: GameState, piece: Piece): (GameState, Option[Piece]) = {
      val (updatedState, pieceIsAlive, updatedEffectStatus) =
        piece.effectStatus.foldLeft((gameState, true, List.empty[EffectStatus])) {
          case (s, effect @ EffectStatus.Petrified(untilTurn)) =>
            s.copy(_3 = (if (untilTurn == currentTurn) Nil else List(effect)) ++ s._3)
          case (s, effect @ EffectStatus.Frozen(untilTurn)) =>
            s.copy(_3 = (if (untilTurn == currentTurn) Nil else List(effect)) ++ s._3)
          case (s, effect @ EffectStatus.Enchanted(untilTurn)) =>
            s.copy(_3 = (if (untilTurn == currentTurn) Nil else List(effect)) ++ s._3)
          case (s, effect @ EffectStatus.WeakEnchanted(untilTurn)) =>
            s.copy(_3 = (if (untilTurn == currentTurn) Nil else List(effect)) ++ s._3)
          case (s, effect @ EffectStatus.Poison(turnOfDeath)) =>
            if (turnOfDeath == currentTurn) {
              s.copy(_2 = false)
            } else
              s.copy(_3 = effect :: s._3)
          case (s, effect @ EffectStatus.InstantKillPositional(distance)) if gameState.getCurrentPlayer.team == piece.team =>
            val gameState = s._1
            (piece.pos + distance).getPiece(gameState.board) match {
              case None =>
                s
              case Some(targetPiece) if targetPiece.team != piece.team => // TODO is there trigger immune?
                val playerMove =
                  if (targetPiece.canBlockFrom(piece.pos)) {
                    PlayerMove.AttackCanBeBlocked(piece, targetPiece)
                  } else {
                    PlayerMove.Attack(piece, targetPiece)
                  }

                s.copy(_1 = gameState.playPlayerMove(playerMove, turnUpdate = false), _3 = effect :: s._3)
            }
          case (s, effect) =>
            s.copy(_3 = effect :: s._3)
        }
      if (pieceIsAlive)
        (updatedState, Some(piece.copy(effectStatus = updatedEffectStatus)))
      else
        (updatedState, None)
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
    stateAfterBlackIsUpdated
  }

  def getCurrentPlayer: Player = if (currentTurn == currentTurn.toInt) playerWhite else playerBlack

  def getNextPlayer: Player = if (currentTurn == currentTurn.toInt) playerBlack else playerWhite

  @inline private final val MaxValue: Int = 1e9.toInt

  def valueOfState(team: PlayerTeam): Int = {
    winner match {
      case PlayerWinType.NotFinished =>
        val whitePoints = playerWhite.morale * 10 + playerWhite.numberOfPieces
        val blackPoints = playerBlack.morale * 10 + playerBlack.numberOfPieces

        team.chooseWhiteBlack(whitePoints - blackPoints, blackPoints - whitePoints)
      case PlayerWinType.PlayerWhite => if (team == White) MaxValue else -MaxValue
      case PlayerWinType.PlayerBlack => if (team == Black) MaxValue else -MaxValue
      case PlayerWinType.Draw => -MaxValue / 2
    }
  }

}

object GameState {

  def compare(before: GameState, after: GameState, team: PlayerTeam): Int =
    after.valueOfState(team)

}
