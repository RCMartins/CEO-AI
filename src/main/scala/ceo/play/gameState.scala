package ceo.play

import ceo.play.GameState.Board
import ceo.play.Player.{PlayerBlack, PlayerWhite}
import ceo.play.PlayerColor.White

import scala.language.implicitConversions

case class BoardPos(row: Int, column: Int) {

  def isValid: Boolean = row >= 0 && row < 8 && column >= 0 && column < 8

  override def toString: String = s"[$row-$column]"

  def getPiece(board: Board): Option[Piece] = if (isValid) board(row)(column) else None

  def isEmpty(board: Board): Boolean = isValid && getPiece(board).isEmpty

  def translate(dx: Int, dy: Int): BoardPos = BoardPos(row + dy, column + dx)

  def posTo(target: BoardPos): Stream[BoardPos] = {
    val rowDiff = target.row - row
    val colDiff = target.column - column
    val dx = if (colDiff == 0) 0 else if (colDiff < 0) -1 else 1
    val dy = if (rowDiff == 0) 0 else if (rowDiff < 0) -1 else 1
    Stream.from(1).map(distance => this.translate(dx * distance, dy * distance)).takeWhile(_ != target) :+ target
  }

  def posUntil(target: BoardPos): Stream[BoardPos] = posTo(target).dropRight(1)
}

object BoardPos {
  implicit def convert(tuple2: (Int, Int)): BoardPos = BoardPos(tuple2._1, tuple2._2)
}

case class Piece(data: PieceData, pos: BoardPos, currentMorale: Int, hasMoved: Boolean) {
  val team: PlayerColor = data.team

  override def toString: String = s"${data.name}$pos"
}

object Piece {
  def apply(data: PieceData, pos: BoardPos): Piece = Piece(data, pos, data.initialMorale, hasMoved = false)
}

case class GameState(board: Board, playerWhite: PlayerWhite, playerBlack: PlayerBlack, currentTurn: Double) {
  def nextTurn: GameState = copy(currentTurn = currentTurn + 0.5)

  override def toString: String = {
    val nameSize = 13
    val dashLine = ("-" * ((nameSize + 1) * 8 + 1)) + "\n"
    board.map(line => line.map { pieceOpt =>
      val formatStr = s"%${nameSize}s"
      formatStr.format(
        pieceOpt.map(
          piece => piece.data.name).getOrElse(""))
    }.mkString("|", "|", "|"))
      .mkString(dashLine, "\n" + dashLine, "\n" + dashLine)
  }

  def placeUnit(piece: Piece): GameState = {
    val piecePlaced =
      copy(
        board = board.updated(piece.pos.row, board(piece.pos.row).updated(piece.pos.column, Some(piece)))
      )

    if (piece.team == White)
      piecePlaced.copy(playerWhite =
        piecePlaced.playerWhite
          .increaseMorale(piece.data.initialMorale)
          .copy(pieces = piece :: playerWhite.pieces)
      )
    else
      piecePlaced.copy(playerBlack =
        piecePlaced.playerBlack
          .increaseMorale(piece.data.initialMorale)
          .copy(pieces = piece :: playerBlack.pieces)
      )
  }

  def getCurrentPlayerMoves: List[PlayerMove] = {
    val currentPlayer: Player = if (currentTurn == currentTurn.toInt) playerWhite else playerBlack

    currentPlayer.pieces.flatMap { piece =>
      val moves: Seq[Moves] = piece.data.moves

      moves.flatMap {
        move =>
          val validMove: Option[PlayerMove] = move.getValidMove(piece, this, currentPlayer)
          validMove
      }
    }
  }
}

object GameState {

  type Board = Vector[Vector[Option[Piece]]]

}