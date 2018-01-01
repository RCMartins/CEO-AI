package ceo.play

import ceo.play.PlayerTeam.{Black, White}

case class GameState(
  board: Board,
  playerWhite: Player,
  playerBlack: Player,
  currentTurn: Double,
  movesHistory: List[PlayerMove],
  boardEffects: List[BoardEffect]
) {

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
      val turnText = s"----- turn: $currentTurn "
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
      else
        List.empty
    }
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

  def playPlayerMove(move: PlayerMove): GameState = {
    import PlayerMove._

    def guardianSwapPiece(state: GameState, piece: Piece): (GameState, Piece) = {
      if (piece.data.isChampion && !piece.data.isImmuneTo(EffectType.Displacement)) {
        state.getPlayer(piece.team).extraData.guardedPositions.get(piece.pos) match {
          case Some(guardianPiece) =>
            val updatedState = playPlayerMove(PlayerMove.Swap(piece, guardianPiece))
            (updatedState, piece.pos.getPiece(updatedState.board).get)
          case None => (state, piece)
        }
      } else {
        (state, piece)
      }
    }

    val newState = move match {
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
        val updatedState2 = piece.afterMagicKill(updatedState1, pieceToDestroy)
        updatedState2
          .removePiece(pieceToDestroy)
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
        val updatedState2 = piece.afterMagicKill(updatedState1, pieceToDestroy)
        updatedState2
          .removePiece(pieceToDestroy)
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
        val updatedState2 = piece.afterMagicKill(updatedState1, pieceToTransform)
        val newPiece = Piece(allyPieceData, pieceToTransform.pos)
        updatedState2
          .changeMorale(piece.team, -moraleCost)
          .updatePiece(pieceToTransform, newPiece)
      case KingDoesCastling(kingPiece, allyPiece, kingTarget, allyTarget) =>
        // TODO: for now this is simplified because it can only affect champions?
        val kingWithoutCastling = DataLoader
          .getPieceData("King-no-Cas", kingPiece.team)
          .createPiece(kingTarget)
          .setMorale(kingPiece.currentMorale)
        this
          .updatePiece(allyPiece, allyPiece.copy(pos = allyTarget))
          .updatePiece(kingPiece, kingWithoutCastling)
      case TeleportPiece(_, _pieceToTeleport, target) =>
        val (updatedState1, pieceToTeleport) = guardianSwapPiece(this, _pieceToTeleport)
        val (updatedState2, pieceToTeleportUpdated) = pieceToTeleport.moveTo(updatedState1, target)
        updatedState2
          .updatePiece(pieceToTeleport, pieceToTeleportUpdated)
      case MagicPush(piece, _pieceToPush, maxPushDistance) => //TODO use MultiMove with Push & Freeze
        val (updatedState1, pieceToPush) = guardianSwapPiece(this, _pieceToPush)
        val pieceToPushPos = pieceToPush.pos
        val dir = (pieceToPushPos - piece.pos).toUnitVector
        val positions = BoardPos.List1to8.view(0, maxPushDistance)
          .map(distance => pieceToPushPos + dir * distance)
          .takeWhile(_.isEmpty(board))

        val (updatedState2, pieceToPushUpdated) = pieceToPush.moveTo(updatedState1, positions.last)
        updatedState2
          .updatePiece(pieceToPush, pieceToPushUpdated)
      case MagicFreeze(piece, _pieceToFreeze, freezeDuration) =>
        val (updatedState, pieceToFreeze) = guardianSwapPiece(this, _pieceToFreeze)
        val pieceToFreezeUpdated = piece.freeze(updatedState, freezeDuration)
        updatedState
          .updatePiece(pieceToFreeze, pieceToFreezeUpdated)
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
          val updatedState2 = piece.afterMagicKill(updatedState1, pieceToKill)
          val pieceUpdated = piece.copy(pos = taurusPos)
          updatedState2
            .removePiece(piece)
            .removePiece(pieceToKill)
            .placePiece(pieceUpdated)
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
          val updatedState = piece.afterMagicKill(updatedState1, pieceToKill)
          val pieceUpdated = piece.copy(pos = taurusPos)
          updatedState
            .removePiece(piece)
            .removePiece(pieceToKill)
            .placePiece(pieceUpdated)
        }
      case MultiMove(move1, move2) =>
        playPlayerMove(move1).playPlayerMove(move2)
      case DummyMove(_) => this
    }

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
  }

  private def startingTurnStatusEffectUpdate: GameState = {

    def updatePieces(pieces: List[Piece]): (List[Piece], List[Piece]) =
      pieces.foldLeft((List.empty[Piece], List.empty[Piece])) {
        case ((deadPieces, updatedPieces), piece) =>
          val updatedEffectStatus = piece.effectStatus.flatMap {
            case effect @ EffectStatus.Petrified(untilTurn) =>
              if (untilTurn == currentTurn) Nil else List(effect)
            case effect @ EffectStatus.Frozen(untilTurn) =>
              if (untilTurn == currentTurn) Nil else List(effect)
            case effect @ EffectStatus.Poison(turnOfDeath) =>
              if (turnOfDeath == currentTurn) {
                return (piece :: deadPieces, updatedPieces)
              } else
                List(effect)
          }
          (deadPieces, piece.copy(effectStatus = updatedEffectStatus) :: updatedPieces)
      }

    def updatePlayer(currentState: GameState, player: Player): (GameState, Player) = {
      val (deadPieces, updatedPieces) = updatePieces(player.piecesAffected)

      // remove all pieces that have effects
      val stateRemovedAll =
        player.piecesAffected.foldLeft(currentState)((state, piece) => state.removePiece(piece))
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
