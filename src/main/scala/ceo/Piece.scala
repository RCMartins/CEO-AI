package ceo

object Piece {

  val BLANK_SQUARE: Piece = new Piece(null)

}

class Piece(val black: Array[Boolean], var isWhite: Boolean) {

  var pieceType: String = ""

  private var blackOccupied: Int = 0

  if (black != null) {
    for (i <- black.indices if black(i)) {
      blackOccupied += 1
    }
  }

  def this(black: Array[Boolean]) = this(black, false)

  def compareWS(other: Piece, maximumWrongPixels: Int): Boolean = {
    var count: Int = 0
    for (i <- black.indices if black(i) ^ other.black(i)) {
       count += 1
      if (count > maximumWrongPixels)
       return false
    }
    true
  }

  def comparePerc(other: Piece, percent: Double): Boolean = {
    val maximumWrongPixels: Int = (blackOccupied * (1.0 - percent)).toInt
    compareWS(other, maximumWrongPixels)
  }

  def setWhite(isWhite: Boolean): Unit = {
    this.isWhite = isWhite
  }

}
