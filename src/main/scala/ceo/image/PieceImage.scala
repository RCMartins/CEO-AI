package ceo.image

import java.awt.image.BufferedImage

final case class PieceImage(bufferedImage: BufferedImage) {

  private lazy val hash = ImageUtils.hashBufferedImage(bufferedImage)

  override def hashCode(): Int = hash

  override def equals(other: Any): Boolean = other match {
    case that: PieceImage =>
      hash == that.hash && ImageUtils.getNumberOfDifferentPixels(bufferedImage, that.bufferedImage) == 0
    case _ => false
  }
}
