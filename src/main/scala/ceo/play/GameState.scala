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

  /**
    * TODO It's possible to draw a game, and it's not that far-fetched...
    * Example: Having a unit killing a bomber unit, if both players reach 0 morale in that play -> draw
    */
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
        val (pieceNewPos, updatedState) = piece.afterMeleeKill(pieceToKill, this)
        updatedState
          .removePiece(pieceToKill)
          .updatePiece(piece, pieceNewPos)
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
      case TransformEnemyIntoAllyUnit(piece, pieceToTransform, moraleCost, allyPieceData) =>
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
      case TaurusRush(piece, pieceToKill, maxDistance) =>
        val pieceToKillPos = pieceToKill.pos
        val direction = Distance(pieceToKillPos.row - piece.pos.row, pieceToKillPos.column - piece.pos.column).toUnitVector
        val positions = BoardPos.List1to8.view(0, maxDistance)
          .map(distance => pieceToKillPos + direction * distance)
          .takeWhile(_.isEmpty(board))
        if (positions.lengthCompare(maxDistance) < 0) {
          // Enemy piece is destroyed, taurus stay at edge of board
          val taurusPos = if (positions.isEmpty) pieceToKill.pos else positions.last
          val updatedState = piece.afterMagicKill(pieceToKill, this)
          val pieceUpdated = piece.copy(pos = taurusPos)
          updatedState
            .removePiece(piece)
            .removePiece(pieceToKill)
            .placePiece(pieceUpdated)
        } else if (positions.forall(_.isEmpty(board))) {
          // Enemy piece is moved, taurus stay one space before enemy piece
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
    )
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
