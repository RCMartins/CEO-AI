package ceo.image

trait PieceImage[A <: PieceImage[A]] {

  def compare(other: A): Double

  def imageMatch(other: A, minPercentageMatch: Double): Boolean = {
    compare(other) >= minPercentageMatch
  }

}

object PieceImage {

  case class PieceImageBlackOnly(blackOpt: Option[Array[Boolean]], isWhite: Option[Boolean] = None) extends PieceImage[PieceImageBlackOnly] {

    override def compare(other: PieceImageBlackOnly): Double = {
      (blackOpt, other.blackOpt) match {
        case (Some(black), Some(blackOther)) =>
          black.zip(blackOther).count { case (b1, b2) => b1 ^ b2 }
        case _ =>
          0.0
      }
    }
  }

}