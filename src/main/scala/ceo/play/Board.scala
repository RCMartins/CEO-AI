package ceo.play

import scala.collection.immutable.VectorIterator

class Board(_board: Vector[Vector[Option[Piece]]]) {

  private val board = _board

  def apply(row: Int, column: Int): Option[Piece] = board(row)(column)

  def getRows: VectorIterator[Vector[Option[Piece]]] = board.iterator

  def remove(row: Int, column: Int): Board =
    new Board(board.updated(row, board(row).updated(column, None)))

  def remove(boardPos: BoardPos): Board =
    new Board(board.updated(boardPos.row, board(boardPos.row).updated(boardPos.column, None)))

  def place(piece: Piece): Board =
    new Board(board.updated(piece.pos.row, board(piece.pos.row).updated(piece.pos.column, Some(piece))))

}

object EmptyBoard extends Board(Vector.fill(8, 8)(None))
