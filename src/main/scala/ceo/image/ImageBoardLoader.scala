package ceo.image

import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO

import ceo.play.PlayerColor

class ImageBoardLoader(imageIn: BufferedImage) extends SimpleImageLoader {

  val width: Int = imageIn.getWidth
  val height: Int = imageIn.getHeight
  private val imageInPixels: Array[Int] = imageIn.getRGB(0, 0, width, height, null, 0, width)

  def this(imageFile: File) = {
    this(ImageIO.read(imageFile))
  }

  val fieldAmount: Array[Array[Array[Int]]] = Array.ofDim[Int](9, 9, 4)
  val fieldLevel: Array[Array[Int]] = Array.ofDim[Int](9, 9)

  val SIZE: Int = 59
  var cornerX = 261
  var cornerY = 86

  def isValid: Boolean = SIZE > 0

  private def squares(x: Int, y: Int): Boolean = {
    val i = y * width + x
    val red = (imageInPixels(i) & 0x00FF0000) >> 16
    val green = (imageInPixels(i) & 0x0000FF00) >> 8
    val blue = (imageInPixels(i) & 0x000000FF) >> 0
    red == 204 && green == 204 && blue == 204
  }

  def currentTeam: PlayerColor = {
    val x = cornerX + 505
    val y = cornerY - 53
    val i = y * width + x
    val red = (imageInPixels(i) & 0x00FF0000) >> 16
    val green = (imageInPixels(i) & 0x0000FF00) >> 8
    val blue = (imageInPixels(i) & 0x000000FF) >> 0
    if (red == 255 && green == 255 && blue == 255) PlayerColor.White else PlayerColor.Black
  }

  def getImageAt(row: Int, column: Int): PieceImage = {
    PieceImage(getImageFrom(imageIn, row, column))
  }

  def getSquare(row: Int, column: Int): Square = {
    val y = cornerY + row * (SIZE + 1)
    val x = cornerX + column * (SIZE + 1)
    new Square(x, y, x + SIZE, y + SIZE)
  }

  private def getImageFrom(imageIn: BufferedImage, row: Int, column: Int): BufferedImage = {
    val square = getSquare(row, column)
    val imageOut = new BufferedImage(SIZE, SIZE, BufferedImage.TYPE_4BYTE_ABGR)
    val imageOutPixels = new Array[Int](SIZE * SIZE)
    val ix = square.left
    val iy = square.top
    for (y <- iy until square.bottom; x <- ix until square.right) {
      val kIn = x + y * width
      val kOut = (x - ix) + (y - iy) * SIZE
      imageOutPixels(kOut) = imageInPixels(kIn)
    }
    imageOut.setRGB(0, 0, SIZE, SIZE, imageOutPixels, 0, SIZE)
    imageOut
  }

}
