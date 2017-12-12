package ceo.play

import ceo.play.GameState.Board
import ceo.play.Player.{PlayerBlack, PlayerWhite}
import ceo.play.PlayerColor.{Black, White}

case class GameState(board: Board, playerWhite: PlayerWhite, playerBlack: PlayerBlack, currentTurn: Double) {
  def nextTurn: GameState = copy(currentTurn = currentTurn + 0.5)

  override def toString: String = {
    val nameSize = 14
    val normalDashLine = "-" * ((nameSize + 1) * 8 + 1)

    def moraleDashLine(morale: Int) = {
      val textInLine = s"morale: ${morale.toString}"
      val firstHalf = normalDashLine.length / 2 - textInLine.length / 2 - 1
      val secondHalf = normalDashLine.length - firstHalf - textInLine.length - 2
      s"${"-" * firstHalf} $textInLine ${"-" * secondHalf}\n"
    }

    board.map(line => line.map { pieceOpt =>
      val formatStr = s"%${nameSize}s"
      formatStr.format(
        pieceOpt.map(
          piece => piece.data.name).getOrElse(""))
    }.mkString("|", "|", "|"))
      .mkString(moraleDashLine(playerBlack.morale), "\n" + normalDashLine + "\n", "\n" + moraleDashLine(playerWhite.morale))
  }

  def placeUnit(piece: Piece): GameState = {
    val withNewPiece =
      copy(
        board = board.updated(piece.pos.row, board(piece.pos.row).updated(piece.pos.column, Some(piece)))
      )

    if (piece.team == White) {
      val updatedPlayerWhite =
        withNewPiece.playerWhite
          .increaseMorale(piece.data.initialMorale)
          .copy(pieces = piece :: withNewPiece.playerWhite.pieces)
      withNewPiece.copy(playerWhite = updatedPlayerWhite)
    } else {
      val updatedPlayerBlack =
        withNewPiece.playerBlack
          .increaseMorale(piece.data.initialMorale)
          .copy(pieces = piece :: withNewPiece.playerBlack.pieces)
      withNewPiece.copy(playerBlack = updatedPlayerBlack)
    }
  }

  def getCurrentPlayerMoves: List[PlayerMove] = {
    val currentPlayer: Player = if (currentTurn == currentTurn.toInt) playerWhite else playerBlack

    currentPlayer.pieces.flatMap { piece =>
      val moves: Seq[Moves] = piece.data.moves

      moves.flatMap(_.getValidMove(piece, this, currentPlayer))
    }
  }

  def isGameOver: Option[PlayerColor] = {
    if (playerWhite.morale == 0) {
      Some(White)
    } else if (playerBlack.morale == 0) {
      Some(Black)
    } else {
      None
    }
  }
}

object GameState {

  type Board = Vector[Vector[Option[Piece]]]

  def compare(before: GameState, after: GameState, team: PlayerColor): Int = {
    after.isGameOver match {
      case Some(playerTeam) if playerTeam == team => Int.MaxValue
      case Some(playerTeam) if playerTeam == team.enemy => Int.MinValue
      case None =>
        val whiteDiff = after.playerWhite.morale - before.playerWhite.morale
        val blackDiff = after.playerBlack.morale - before.playerBlack.morale

        team.chooseWhiteBlack(whiteDiff - blackDiff, blackDiff - whiteDiff)
    }
  }

}
