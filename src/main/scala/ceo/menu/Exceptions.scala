package ceo.menu

import ceo.play.Piece

object Exceptions {

  class BoardStartsWithUnknownPieces(unknownPieces: Seq[Piece]) extends Exception

  class AllPiecesAreKnown extends Exception

  class GameIsOverByAbandonOrTimeout extends Exception

}
