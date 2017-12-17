package ceo.play

import ceo.play.Player.{PlayerBlack, PlayerWhite}
import ceo.play.PlayerTeam.{Black, White}

case class GameState(board: Board, playerWhite: PlayerWhite, playerBlack: PlayerBlack, currentTurn: Double, movesHistory: List[PlayerMove]) {

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
      piece.data.moves
        .flatMap(_.getValidMove(piece, this, currentPlayer))
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
          .removePiece(piece)
          .removePiece(pieceToKill)
          .placePiece(pieceUpdated)
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
      case TaurusRush(piece, pieceToKill, maxDistance) =>
        val dir = (pieceToKill.pos - piece.pos).normalize
        val positions = pieceToKill.pos.posTo(pieceToKill.pos + dir * maxDistance).filter(_.isValid).toList
        if (positions.lengthCompare(maxDistance) < 0) {
          // Enemy piece is destroyed, taurus stay at edge of board
          val taurusPos = positions.last
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
            pos = positions.last
            // TODO: does this count towards 'hasMoved' ???
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
          val taurusPos = positions.find(_.nonEmpty(board)).get - dir
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
      case Some(winningTeam) if winningTeam == team => MaxValue
      case Some(winningTeam) if winningTeam == team.enemy => -MaxValue
      case None =>
        val whitePoints = playerWhite.morale
        val blackPoints = playerBlack.morale

        team.chooseWhiteBlack(whitePoints - blackPoints, blackPoints - whitePoints)
    }
  }

}

object GameState {

  def compare(before: GameState, after: GameState, team: PlayerTeam): Int =
    after.valueOfState(team)

}
