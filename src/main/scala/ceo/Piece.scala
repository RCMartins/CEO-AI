package ceo

object Piece {

  val BLANK_SQUARE: Piece = new Piece(None)

}

class Piece(val blackOpt: Option[Array[Boolean]], var isWhite: Boolean) {

  var pieceType: String = ""

  private var blackOccupied: Int = 0

  blackOpt.foreach(black =>
    for (i <- black.indices if black(i)) {
      blackOccupied += 1
    }
  )

  def this(black: Option[Array[Boolean]]) = this(black, false)

  def compareWS(other: Piece, maximumWrongPixels: Int): Boolean = {
    (blackOpt, other.blackOpt) match {
      case (Some(black), Some(blackOther)) =>
        black.zip(blackOther).count{ case (b1, b2) => b1 ^ b2 } <= maximumWrongPixels
      case _ =>
        false
    }
  }

  def comparePerc(other: Piece, percent: Double): Boolean = {
    val maximumWrongPixels: Int = (blackOccupied * (1.0 - percent)).toInt
    compareWS(other, maximumWrongPixels)
  }

  def setWhite(isWhite: Boolean): Unit = {
    this.isWhite = isWhite
  }

}
