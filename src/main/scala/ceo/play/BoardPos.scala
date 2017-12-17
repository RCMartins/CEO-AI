package ceo.play

import scala.language.implicitConversions

class BoardPos private(val row: Int, val column: Int) {

  import BoardPos.List1to8

  @inline def +(other: BoardPos): BoardPos = BoardPos(row + other.row, column + other.column)

  @inline def -(other: BoardPos): BoardPos = BoardPos(row - other.row, column - other.column)

  @inline def *(multiplier: Int): BoardPos = BoardPos(row * multiplier, column * multiplier)

  def normalize: BoardPos = {
    val normalizedRow = if (row > 0) 1 else if (row < 0) -1 else 0
    val normalizedColumn = if (column > 0) 1 else if (column < 0) -1 else 0
    BoardPos(normalizedRow, normalizedColumn)
  }

  override def toString: String = s"($row, $column)"

  @inline def isValid: Boolean = row >= 0

  @inline def getPiece(board: Board): Option[Piece] = if (isValid) board(row, column) else None

  @inline def isEmpty(board: Board): Boolean = isValid && getPiece(board).isEmpty

  @inline def nonEmpty(board: Board): Boolean = !isValid || getPiece(board).nonEmpty

  @inline def translate(dx: Int, dy: Int): BoardPos = BoardPos(row + dy, column + dx)

//  def posTo(target: BoardPos): Stream[BoardPos] = {
//    val rowDiff = target.row - row
//    val colDiff = target.column - column
//    val dx = if (colDiff == 0) 0 else if (colDiff < 0) -1 else 1
//    val dy = if (rowDiff == 0) 0 else if (rowDiff < 0) -1 else 1
//    List1to8.view.map(distance => this.translate(dx * distance, dy * distance)).takeWhile(_ != target) :+ target
//  }

  @inline def allPosToAreEmpty(target: BoardPos, board: Board): Boolean = {
    target.isEmpty(board) && allPosUntilAreEmpty(target, board)
  }

  @inline def allPosUntilAreEmpty(target: BoardPos, board: Board): Boolean = {
    val rowDiff = target.row - row
    val colDiff = target.column - column
    val dx = if (colDiff == 0) 0 else if (colDiff < 0) -1 else 1
    val dy = if (rowDiff == 0) 0 else if (rowDiff < 0) -1 else 1
    val maxDist = Math.max(Math.abs(colDiff), Math.abs(rowDiff)) - 1
    List1to8.view(0, maxDist).forall(distance => BoardPos(row + dy * distance, column + dx * distance).isEmpty(board))
  }
}

object BoardPos {
  val List1to8: List[Int] = (1 to 8).toList

  @inline implicit def convert(tuple2: (Int, Int)): BoardPos = BoardPos(tuple2._1, tuple2._2)

  private val cache: Array[BoardPos] = (for {
    row <- 0 until 8
    column <- 0 until 8
  } yield new BoardPos(row, column)).toArray ++ Array(new BoardPos(-1, -1))

  def apply(row: Int, column: Int): BoardPos = {
    val index = row * 8 + column
    if (index < 0 || index >= 64) cache(64) else cache(index)
  }
}
