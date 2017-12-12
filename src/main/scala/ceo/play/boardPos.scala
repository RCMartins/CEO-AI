package ceo.play

import ceo.play.GameState.Board

import scala.language.implicitConversions

case class BoardPos(row: Int, column: Int) {

  override def toString: String = s"[$row-$column]"

  def isValid: Boolean = row >= 0 && row < 8 && column >= 0 && column < 8

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
  def team: PlayerColor = data.team

  override def toString: String = s"${data.name}$pos"
}

object Piece {
  def apply(data: PieceData, pos: BoardPos): Piece = Piece(data, pos, data.initialMorale, hasMoved = false)
}
