package ceo.menu

import ceo.play.Piece

object Exceptions {

  class BoardStartsWithUnknownPieces(unknownPieces: List[Piece]) extends Exception

}
