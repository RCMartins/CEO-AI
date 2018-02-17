package ceo.image

import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO

import ceo.control.MainControl
import ceo.play.PlayerColor

class ImageBoardLoader(val imageIn: BufferedImage) extends SimpleImageLoader {

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

  def isValid: Boolean = {
    def testPixel(pixel: Int, red: Int, green: Int, blue: Int, debug: Boolean = false): Boolean = {
      val redPart = (pixel & 0x00FF0000) >> 16
      val greenPart = (pixel & 0x0000FF00) >> 8
      val bluePart = (pixel & 0x000000FF) >> 0
      if (debug)
        println(s"($pixel) ($red, $redPart), ($green, $greenPart), ($blue, $bluePart)")
      red == redPart && green == greenPart && blue == bluePart
    }

    def at(pixels: Array[Int], x: Int, y: Int): Int = {
      pixels(x + y * MainControl.sizeX)
    }

    testPixel(at(imageInPixels, 378, 4), 0, 0, 0) && // PlayingMatch
      testPixel(at(imageInPixels, 517, 3), 0, 0, 0) && // PlayingMatch
      testPixel(at(imageInPixels, 617, 4), 0, 0, 0) && // PlayingMatch
      testPixel(at(imageInPixels, 284, 643), 0, 0, 0) && // PlayingMatch
      testPixel(at(imageInPixels, 333, 642), 0, 0, 0) // PlayingMatch
  }

  def currentTeam(mainPlayer: PlayerColor): PlayerColor = {
    val x = 765
    val y = 40
    val i = y * width + x
    val red = (imageInPixels(i) & 0x00FF0000) >> 16
    val green = (imageInPixels(i) & 0x0000FF00) >> 8
    val blue = (imageInPixels(i) & 0x000000FF) >> 0
    if (red == 219 && green == 219 && blue == 219) {
      val x2 = 787
      val y2 = 34
      val i2 = y2 * width + x2
      val red2 = (imageInPixels(i2) & 0x00FF0000) >> 16
      val green2 = (imageInPixels(i2) & 0x0000FF00) >> 8
      val blue2 = (imageInPixels(i2) & 0x000000FF) >> 0
      if (red2 == 45 && green2 == 45 && blue2 == 45) {
        PlayerColor.Black
      } else {
        mainPlayer
      }
    } else if (red == 255 && green == 255 && blue == 255) {
      PlayerColor.White
    } else {
      Console.err.println("Wrong color on turn check!")
      PlayerColor.White
    }
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
