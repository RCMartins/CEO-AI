package ceo.play

import ceo.play.PlayerTeam.{Black, White}

case class GameState(board: Board, playerWhite: Player, playerBlack: Player, currentTurn: Double, movesHistory: List[PlayerMove]) {

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

    currentPlayer.pieces.flatMap { piece =>
      if (piece.isNotFrozenOrPetrified)
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
    if (playerWhite.morale == 0 && playerBlack.morale == 0) {
      PlayerWinType.Draw
    } else if (playerWhite.morale == 0) {
      PlayerWinType.PlayerBlack
    } else if (playerBlack.morale == 0) {
      PlayerWinType.PlayerWhite
    } else {
      PlayerWinType.NotFinished
    }
  }

  def playPlayerMove(move: PlayerMove): GameState = {
    import PlayerMove._

    val newState = move match {
      // Standard moves:
      case Move(piece, target) =>
        val pieceNewPos = piece.moveTo(target, this)
        updatePiece(piece, pieceNewPos)
      case Attack(piece, pieceToKill) =>
        val (pieceNewPosOpt, updatedState) = piece.afterMeleeKill(pieceToKill, this)
        val updatedStateAfterRemoves =
          updatedState
            .removePiece(pieceToKill)
            .removePiece(piece)
        pieceNewPosOpt match {
          case Some(pieceNewPos) => updatedStateAfterRemoves.placePiece(pieceNewPos)
          case _ => updatedStateAfterRemoves
        }
      case Swap(piece, pieceToSwap) =>
        val updatedState = this.removePiece(piece).removePiece(pieceToSwap)

        val pieceToSwapUpdated = pieceToSwap.moveTo(piece.pos, updatedState)
        val pieceUpdated = piece.moveTo(pieceToSwap.pos, updatedState)

        updatedState
          .placePiece(pieceUpdated)
          .placePiece(pieceToSwapUpdated)
      // Ranged moves:
      case RangedDestroy(piece, pieceToKill) =>
        val updatedState = piece.afterMagicKill(pieceToKill, this)
        updatedState
          .removePiece(pieceToKill)
      case RangedPetrify(piece, pieceToPetrify, turnsPetrified) =>
        val pieceToPetrifyUpdated = pieceToPetrify.petrify(this, turnsPetrified)
        this
          .updatePiece(pieceToPetrify, pieceToPetrifyUpdated)
      case RangedPush(piece, pieceToPush, moraleCost, maxPushDistance) =>
        val pieceToPushPos = pieceToPush.pos
        val dir = (pieceToPushPos - piece.pos).toUnitVector
        val positions = BoardPos.List1to8.view(0, maxPushDistance)
          .map(distance => pieceToPushPos + dir * distance)
          .takeWhile(_.isEmpty(board))

        val pieceToPushUpdated = pieceToPush.moveTo(positions.last, this)
        this
          .changeMorale(piece.team, -moraleCost)
          .updatePiece(pieceToPush, pieceToPushUpdated)
      // Magic moves:
      case MagicDestroy(piece, pieceToKill) =>
        val updatedState = piece.afterMagicKill(pieceToKill, this)
        updatedState
          .removePiece(pieceToKill)
      case MagicPoison(piece, pieceToPoison, turnsToDeath) =>
        val (updatedState, updatedPiece, updatedPoisonedPiece) = piece.afterPoisonPiece(pieceToPoison, turnsToDeath, this)
        updatedState
          .updatePiece(piece, updatedPiece)
          .updatePiece(pieceToPoison, updatedPoisonedPiece)
      case MagicCharm(piece, pieceToCharm) =>
        val pieceToCharmUpdated = pieceToCharm.swapTeams
        this
          .updatePiece(pieceToCharm, pieceToCharmUpdated)
      case TransformEnemyIntoAllyPiece(piece, pieceToTransform, moraleCost, allyPieceData) =>
        val updatedState = piece.afterMagicKill(pieceToTransform, this)
        val newPiece = Piece(allyPieceData, pieceToTransform.pos)
        updatedState
          .changeMorale(piece.team, -moraleCost)
          .updatePiece(pieceToTransform, newPiece)
      case KingDoesCastling(kingPiece, allyPiece, kingTarget, allyTarget) =>
        // TODO: for now this is simplified because it can only affect champions?
        val KingWithoutCastling = DataLoader
          .getPieceData("King-no-Cas", kingPiece.team)
          .createPiece(kingTarget)
          .setMorale(kingPiece.currentMorale)
        this
          .updatePiece(allyPiece, allyPiece.copy(pos = allyTarget))
          .updatePiece(kingPiece, KingWithoutCastling)
      //TODO: MagicPushFreeze
      case TaurusRush(piece, pieceToKill, maxDistance) =>
        val pieceToKillPos = pieceToKill.pos
        val direction = Distance(pieceToKillPos.row - piece.pos.row, pieceToKillPos.column - piece.pos.column).toUnitVector
        val positions = BoardPos.List1to8.take(maxDistance)
          .map(distance => pieceToKillPos + direction * distance)
          .takeWhile(_.isEmpty(board))
        if (positions.lengthCompare(maxDistance) < 0) {
          // Enemy piece is destroyed, taurus stays at edge of board
          val taurusPos = if (positions.isEmpty) pieceToKill.pos else positions.last
          val updatedState = piece.afterMagicKill(pieceToKill, this)
          val pieceUpdated = piece.copy(pos = taurusPos)
          updatedState
            .removePiece(piece)
            .removePiece(pieceToKill)
            .placePiece(pieceUpdated)
        } else if (positions.forall(_.isEmpty(board))) {
          // Enemy piece is moved, taurus stays one space before enemy piece
          val pieceToKillUpdated = pieceToKill.copy(pos = positions.last)
          val pieceUpdated = piece.copy(pos = positions.init.last)
          this
            .removePiece(piece)
            .removePiece(pieceToKill)
            .placePiece(pieceUpdated)
            .placePiece(pieceToKillUpdated)
        } else {
          // Enemy piece is crushed, taurus stays on the last empty space
          val taurusPos = positions.find(_.nonEmpty(board)).get - direction
          val updatedState = piece.afterMagicKill(pieceToKill, this)
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
