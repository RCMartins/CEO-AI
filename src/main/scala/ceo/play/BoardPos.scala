package ceo.play

import scala.language.implicitConversions

case class BoardPos(row: Int, column: Int) {

  @inline def +(other: BoardPos): BoardPos = BoardPos(row + other.row, column + other.column)

  @inline def -(other: BoardPos): BoardPos = BoardPos(row - other.row, column - other.column)

  @inline def *(multiplier: Int): BoardPos = BoardPos(row * multiplier, column * multiplier)

  def normalize: BoardPos = {
    val normalizedRow = if (row > 0) 1 else if (row < 0) -1 else 0
    val normalizedColumn = if (column > 0) 1 else if (column < 0) -1 else 0
    BoardPos(normalizedRow, normalizedColumn)
  }

  override def toString: String = s"($row, $column)"

  @inline def isValid: Boolean = row >= 0 && row < 8 && column >= 0 && column < 8

  @inline def getPiece(board: Board): Option[Piece] = if (isValid) board(row, column) else None

  @inline def isEmpty(board: Board): Boolean = isValid && getPiece(board).isEmpty

  @inline def nonEmpty(board: Board): Boolean = !isValid || getPiece(board).nonEmpty

  @inline def translate(dx: Int, dy: Int): BoardPos = BoardPos(row + dy, column + dx)

  def posTo(target: BoardPos): Stream[BoardPos] = {
    val rowDiff = target.row - row
    val colDiff = target.column - column
    val dx = if (colDiff == 0) 0 else if (colDiff < 0) -1 else 1
    val dy = if (rowDiff == 0) 0 else if (rowDiff < 0) -1 else 1
    Stream.from(1).map(distance => this.translate(dx * distance, dy * distance)).takeWhile(_ != target) :+ target
  }

  def posUntil(target: BoardPos): Stream[BoardPos] = {
    val rowDiff = target.row - row
    val colDiff = target.column - column
    val dx = if (colDiff == 0) 0 else if (colDiff < 0) -1 else 1
    val dy = if (rowDiff == 0) 0 else if (rowDiff < 0) -1 else 1
    Stream.from(1).map(distance => this.translate(dx * distance, dy * distance)).takeWhile(_ != target)
  }
}

object BoardPos {
  @inline implicit def convert(tuple2: (Int, Int)): BoardPos = BoardPos(tuple2._1, tuple2._2)
}
