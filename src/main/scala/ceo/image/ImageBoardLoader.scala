package ceo.image

import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO

import ImageBoardLoader.DEBUG_MODE
import ImageUtils.writeImage
import ceo.play.PlayerTeam

object ImageBoardLoader {

  val DEBUG_MODE: Boolean = false

  def main(args: Array[String]): Unit = {
    val time = System.currentTimeMillis()
    new ImageBoardLoader(new File("Images/challenge-2018-01-03.png"))
    //        new ImageBoardLoader(new File("Images/challenge-2017-12-30.png"))
    //    new ImageBoardLoader(new File("Images/print_test2.png"))
    println("Total Time: " + (System.currentTimeMillis() - time))
  }
}

class ImageBoardLoader(imageIn: BufferedImage) extends SimpleImageLoader {

  val SIZE: Int = 61
  val width: Int = imageIn.getWidth
  val height: Int = imageIn.getHeight
  private val imageInPixels: Array[Int] = imageIn.getRGB(0, 0, width, height, null, 0, width)

  def this(imageFile: File) = {
    this(ImageIO.read(imageFile))
  }

  private def squares(x: Int, y: Int): Boolean = {
    val i = y * width + x
    val red = (imageInPixels(i) & 0x00FF0000) >> 16
    val green = (imageInPixels(i) & 0x0000FF00) >> 8
    val blue = (imageInPixels(i) & 0x000000FF) >> 0
    red == 204 && green == 204 && blue == 204
  }

  val fieldAmount: Array[Array[Array[Int]]] = Array.ofDim[Int](9, 9, 4)
  val fieldLevel: Array[Array[Int]] = Array.ofDim[Int](9, 9)

  updateSquareSize(imageIn)

  def isValid: Boolean = SIZE > 0

  def currentTeam: PlayerTeam = {
    val x = Square.INIT_X + 505
    val y = Square.INIT_Y - 53
    val i = y * width + x
    val red = (imageInPixels(i) & 0x00FF0000) >> 16
    val green = (imageInPixels(i) & 0x0000FF00) >> 8
    val blue = (imageInPixels(i) & 0x000000FF) >> 0
    if (red == 255 && green == 255 && blue == 255) PlayerTeam.White else PlayerTeam.Black
  }

  def updateSquareSize(imageIn: BufferedImage): Unit = {
    var CORNER_X = 0
    var CORNER_Y = 0
    val width = imageIn.getWidth
    val height = imageIn.getHeight
    var ix = 0
    var iy = 0

    def findBegin(): Unit = {
      for {
        y <- 0 until height
        x <- 0 until width - 200 by 200
        if squares(x, y) && squares(x + 200, y)
      } {
        iy = y
        ix = 0
        return
      }
    }

    findBegin()

    def findEnd(): Unit = {
      for {
        y <- iy until height
        x <- ix until width
        if squares(x, y) && squares(x + 1, y) && squares(x + 2, y)
        if squares(x, y + 1) && squares(x, y + 2)
        if !squares(x - 1, y) && !squares(x - 1, y + 1)
      } {
        CORNER_X = x
        CORNER_Y = y
        return
      }
    }

    findEnd()

    CORNER_X += 6
    CORNER_Y += 5

    Square.INIT_X = CORNER_X
    Square.INIT_Y = CORNER_Y
  }

  def getImageAt(row: Int, column: Int): PieceImage = {
    PieceImage(getImageFrom(imageIn, row, column))
  }

  def getSquare(row: Int, column: Int): Square = {
    val y = Square.INIT_Y + row * (SIZE - 1)
    val x = Square.INIT_X + column * (SIZE - 1)
    new Square(x, y, x + SIZE - 1, y + SIZE - 1)
  }

  private def getImageFrom(imageIn: BufferedImage, row: Int, column: Int): BufferedImage = {
    val square = getSquare(row, column)
    val imageInWidth = imageIn.getWidth
    val sqWidth = square.width - 2
    val sqHeight = square.height - 2
    val imageOut = new BufferedImage(sqWidth, sqHeight, BufferedImage.TYPE_4BYTE_ABGR)
    val imageOutPixels = new Array[Int](sqWidth * sqHeight)
    val ix = square.left + 1
    val iy = square.top + 1
    for (y <- iy until square.bottom; x <- ix until square.right) {
      val kIn = x + y * imageInWidth
      val kOut = (x - ix) + (y - iy) * sqWidth
      imageOutPixels(kOut) = imageInPixels(kIn)
    }
    imageOut.setRGB(0, 0, sqWidth, sqHeight, imageOutPixels, 0, sqWidth)
    imageOut
  }

}
