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
        piece.data.moves
          .flatMap(_.getValidMove(piece, this, currentPlayer))
      else
        List.empty
    }
  }

  def winner: Option[PlayerTeam] = {
    if (playerWhite.morale == 0) {
      Some(Black)
    } else if (playerBlack.morale == 0) {
      Some(White)
    } else {
      None
    }
  }

  def playPlayerMove(move: PlayerMove): GameState = {
    import PlayerMove._

    val newState = move match {
      case Move(piece, target) =>
        val pieceNewPos = piece.moveTo(target, this).copy(hasMoved = true)
        updatePiece(piece, pieceNewPos)
      case Attack(piece, pieceToKill) =>
        val (pieceNewPos, updatedState) = piece.killed(pieceToKill, this)
        val pieceUpdated = pieceNewPos.copy(hasMoved = true)

        updatedState
          .removePiece(pieceToKill)
          .updatePiece(piece, pieceUpdated)
      case Swap(piece, pieceToSwap) =>
        val updatedState = this.removePiece(piece).removePiece(pieceToSwap)

        val pieceToSwapUpdated = pieceToSwap.moveTo(piece.pos, updatedState).copy(hasMoved = true)
        val pieceUpdated = piece.moveTo(pieceToSwap.pos, updatedState).copy(hasMoved = true)

        updatedState
          .placePiece(pieceUpdated)
          .placePiece(pieceToSwapUpdated)
      case RangedDestroy(piece, pieceToKill) =>
        val updatedState = pieceToKill.destroyed(this)
        val pieceUpdated = piece.copy(hasMoved = true)
        updatedState
          .removePiece(pieceToKill)
          .updatePiece(piece, pieceUpdated)
      case MagicDestroy(piece, pieceToKill) =>
        val updatedState = pieceToKill.destroyed(this)
        val pieceUpdated = piece.copy(hasMoved = true)
        updatedState
          .removePiece(pieceToKill)
          .updatePiece(piece, pieceUpdated)
      case RangedPetrify(piece, pieceToPetrify, turnsPetrified) =>
        val pieceToPetrifyUpdated = pieceToPetrify.petrify(this, turnsPetrified)
        val pieceUpdated = piece.copy(hasMoved = true)
        this
          .updatePiece(pieceToPetrify, pieceToPetrifyUpdated)
          .updatePiece(piece, pieceUpdated)
      case TransformEnemyIntoAllyUnit(piece, pieceToTransform, moraleCost, allyPieceData) =>
        val updatedState = pieceToTransform.destroyed(this)
        val pieceUpdated = piece.copy(hasMoved = true)
        val newPiece = Piece(allyPieceData, pieceToTransform.pos)
        updatedState
          .changeMorale(piece.team, -moraleCost)
          .updatePiece(pieceToTransform, newPiece)
          .updatePiece(piece, pieceUpdated)
      case MultiMove(move1, move2) =>
        playPlayerMove(move1).playPlayerMove(move2)
      case KingDoesCastling(kingPiece, allyPiece, kingTarget, allyTarget) =>
        // TODO: for now this is simplified because it can only affect champions?
        val KingWithoutCastling = DataLoader
          .getPieceData("King-no-Cas", kingPiece.team)
          .createPiece(kingTarget)
          .setMorale(kingPiece.currentMorale)
        this
          .updatePiece(allyPiece, allyPiece.copy(pos = allyTarget))
          .updatePiece(kingPiece, KingWithoutCastling)
      case TaurusRush(piece, pieceToKill, maxDistance) =>

        val pieceToKillPos = pieceToKill.pos
        val direction = Distance( pieceToKillPos.row - piece.pos.row,  pieceToKillPos.column - piece.pos.column).toUnitVector
        val positions = BoardPos.List1to8.view(0, maxDistance)
          .map(distance => pieceToKillPos + direction * distance)
          .takeWhile(_.isEmpty(board))
        if (positions.lengthCompare(maxDistance) < 0) {
          // Enemy piece is destroyed, taurus stay at edge of board
          val taurusPos = if (positions.isEmpty) pieceToKill.pos else positions.last
          val updatedState = pieceToKill.destroyed(this)
          val pieceUpdated = piece.copy(
            pos = taurusPos,
            hasMoved = true
          )
          updatedState
            .removePiece(piece)
            .removePiece(pieceToKill)
            .placePiece(pieceUpdated)
        } else if (positions.forall(_.isEmpty(board))) {
          // Enemy piece is moved, taurus stay one space before enemy piece
          val pieceToKillUpdated = pieceToKill.copy(
            pos = positions.last,
            hasMoved = true
          )
          val pieceUpdated = piece.copy(
            pos = positions.init.last,
            hasMoved = true
          )
          this
            .removePiece(piece)
            .removePiece(pieceToKill)
            .placePiece(pieceUpdated)
            .placePiece(pieceToKillUpdated)
        } else {
          // Enemy piece is crushed, taurus stays on the last empty space
          val taurusPos = positions.find(_.nonEmpty(board)).get - direction
          val updatedState = pieceToKill.destroyed(this)
          val pieceUpdated = piece.copy(
            pos = taurusPos,
            hasMoved = true
          )
          updatedState
            .removePiece(piece)
            .removePiece(pieceToKill)
            .placePiece(pieceUpdated)
        }
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
    )
  }

  def getCurrentPlayer: Player = if (currentTurn == currentTurn.toInt) playerWhite else playerBlack

  def getNextPlayer: Player = if (currentTurn == currentTurn.toInt) playerBlack else playerWhite

  def valueOfState(team: PlayerTeam): Int = {
    val MaxValue = 1e9.toInt

    winner match {
      case None =>
        val whitePoints = playerWhite.morale * 10 + playerWhite.numberOfPieces
        val blackPoints = playerBlack.morale * 10 + playerBlack.numberOfPieces

        team.chooseWhiteBlack(whitePoints - blackPoints, blackPoints - whitePoints)
      case Some(winningTeam) if winningTeam == team => MaxValue
      case Some(winningTeam) if winningTeam != team => -MaxValue
    }
  }

}

object GameState {

  def compare(before: GameState, after: GameState, team: PlayerTeam): Int =
    after.valueOfState(team)

}
