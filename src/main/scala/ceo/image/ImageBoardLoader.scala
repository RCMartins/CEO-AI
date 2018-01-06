package ceo.image

import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO

import ImageBoardLoader.DEBUG_MODE
import ImageUtils.writeImage

object ImageBoardLoader {

  val DEBUG_MODE: Boolean = false
  }
}

class ImageBoardLoader(imageIn: BufferedImage) {

  def this(imageFile: File) = {
    this(ImageIO.read(imageFile))
  }

  var SIZE: Int = _
  private var squares: Array[Array[Boolean]] = _
  private val LIMIT_TO_MAKE_BLACK: Int = 150

  val fieldAmount: Array[Array[Array[Int]]] = Array.ofDim[Int](9, 9, 4)
  val fieldLevel: Array[Array[Int]] = Array.ofDim[Int](9, 9)

  val (cornerCheck, black, levels) = findSquareBorders(imageIn)

  def findSquareBorders(imageIn: BufferedImage): (BufferedImage, BufferedImage, BufferedImage) = {
    val width = imageIn.getWidth
    val height = imageIn.getHeight
    val imageInPixels = imageIn.getRGB(0, 0, width, height, null, 0, width)

    val cornerCheck: BufferedImage = {
      val imageOut = new BufferedImage(width, height, BufferedImage.TYPE_4BYTE_ABGR)
      val imageOutPixels = Array.ofDim[Int](imageInPixels.length)
      squares = Array.ofDim[Boolean](width, height)
      for (i <- 0 until imageInPixels.length) {
        val red = (imageInPixels(i) & 0x00FF0000) >> 16
        val green = (imageInPixels(i) & 0x0000FF00) >> 8
        val blue = (imageInPixels(i) & 0x000000FF) >> 0
        if (red == 204 && green == 204 && blue == 204) {
          imageOutPixels(i) = imageInPixels(i)
          val x = i % width
          val y = i / width
          squares(x)(y) = true
        }
      }
      imageOut.setRGB(0, 0, width, height, imageOutPixels, 0, width)
      if (DEBUG_MODE)
        writeImage(imageOut, "PRINTS/temp/cornerCheck.png")
      imageOut
    }

    updateSquareSize(imageIn)
    val black: BufferedImage = {
      val imageOut = new BufferedImage(width, height, BufferedImage.TYPE_4BYTE_ABGR)
      val imageOutPixels = new Array[Int](imageInPixels.length)

      for (iy <- 0 until 8 * SIZE; ix <- 0 until 8 * SIZE) {
        val i = Square.INIT_X + ix + (Square.INIT_Y + iy) * width
        val red = (imageInPixels(i) & 0x00FF0000) >> 16
        val green = (imageInPixels(i) & 0x0000FF00) >> 8
        val blue = (imageInPixels(i) & 0x000000FF) >> 0
        if (red <= LIMIT_TO_MAKE_BLACK && green == red && blue == red) {
          val out = imageInPixels(i) & 0xFF000000
          imageOutPixels(i) = out
        }
      }
      imageOut.setRGB(0, 0, width, height, imageOutPixels, 0, width)
      if (DEBUG_MODE)
        writeImage(imageOut, "PRINTS/temp/black.png")
      imageOut
    }

    val levels: BufferedImage = {
      val imageOut = new BufferedImage(width, height, BufferedImage.TYPE_4BYTE_ABGR)
      val imageOutPixels = new Array[Int](imageInPixels.length)

      for (iy <- 0 until 8 * SIZE; ix <- 0 until 8 * SIZE) {
        val i = Square.INIT_X + ix + (Square.INIT_Y + iy) * width
        val red = (imageInPixels(i) & 0x00FF0000) >> 16
        val green = (imageInPixels(i) & 0x0000FF00) >> 8
        val blue = (imageInPixels(i) & 0x000000FF) >> 0
        val x = ix / SIZE + 1
        val y = iy / SIZE + 1
        var level = 0
        if (green == blue && green + 120 < red) {
          imageOutPixels(i) = imageInPixels(i)
          level = 3
        } else if (Math.abs(red - blue) <= 10 && green + 80 < red) {
          imageOutPixels(i) = imageInPixels(i)
          level = 1
        } else if (blue + 20 < green && green < 40 & red > 100) {
          imageOutPixels(i) = imageInPixels(i)
          level = 2
        }
        if (level != 0)
          fieldAmount(x)(y)(level) += 1
      }
      for (y <- 1 to 8; x <- 1 to 8) {
        val levels: Array[Int] = fieldAmount(x)(y)
        val greatestLevel = levels.zipWithIndex.maxBy(_._1)._2
        fieldLevel(x)(y) = greatestLevel
      }
      imageOut.setRGB(0, 0, width, height, imageOutPixels, 0, width)
      if (DEBUG_MODE)
        writeImage(imageOut, "PRINTS/temp/special.png")
      imageOut
    }
    (cornerCheck, black, levels)
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
        x <- 0 until width - 50 by 50
        if squares(x)(y) && squares(x + 50)(y)
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
        if squares(x)(y) && squares(x + 1)(y) && squares(x + 2)(y)
        if squares(x)(y + 1) && squares(x)(y + 2)
        if !squares(x - 1)(y) && !squares(x - 1)(y + 1)
      } {
        CORNER_X = x
        CORNER_Y = y
        return
      }
    }

    findEnd()

    //println(CORNER_X + " " + CORNER_Y)
    CORNER_X += 6
    CORNER_Y += 5
    SIZE = 0

    def something(): Unit = {
      for {
        x <- CORNER_X + 1 until width
        y = CORNER_Y + 1
        if squares(x)(y) && squares(x)(y + 1) && squares(x)(y + 2) && squares(x)(y + 3)
      } {
        SIZE = x - CORNER_X + 1
        return
      }
    }

    something()

    Square.INIT_X = CORNER_X
    Square.INIT_Y = CORNER_Y
  }

  //  def getPieceImageBlackAt(row: Int, column: Int): PieceImage.PieceImageBlackOnly = {
  //    getPieceFromBlack(row, column)
  //  }

  def getImageWithTiersAt(row: Int, column: Int): BufferedImage = {
    //    ImageUtils.joinBufferedImages(getImageFromBlack(row, column), getImageFromTiers(row, column))
    getImageFrom(imageIn, row, column)
  }

  def getSquare(row: Int, column: Int): Square = {
    val y = Square.INIT_Y + row * (SIZE - 1)
    val x = Square.INIT_X + column * (SIZE - 1)
    new Square(x, y, x + SIZE - 1, y + SIZE - 1)
  }

  def getPieceFromBlack(row: Int, column: Int): PieceImage.PieceImageBlackOnly = {
    val imageIn: BufferedImage = black
    val square = getSquare(row, column)
    val imageInWidth = imageIn.getWidth
    val imageInPixels = imageIn.getRGB(0, 0, imageInWidth, imageIn.getHeight, null, 0, imageInWidth)
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
    val piece = buildPiece(imageOutPixels, sqWidth)
    //    writeImage(imageOut, "SQUARES/" + i + "-" + j + ".png")
    //    Thread.sleep(500)
    piece
  }

  private def buildPiece(imageOutPixels: Array[Int], sqWidth: Int): PieceImage.PieceImageBlackOnly = {
    val black = new Array[Boolean](imageOutPixels.length)
    var blackCount = 0
    for (i <- imageOutPixels.indices) {
      if (imageOutPixels(i) == 0) black(i) = false //System.out.print("  ");
      else {
        black(i) = true //System.out.print("# ");

        blackCount += 1
      }
    }
    PieceImage.PieceImageBlackOnly(Some(black))
  }

  def getImageFrom(imageIn: BufferedImage, row: Int, column: Int): BufferedImage = {
    val square = getSquare(row, column)
    val imageInWidth = imageIn.getWidth
    val imageInPixels: Array[Int] = imageIn.getRGB(0, 0, imageInWidth, imageIn.getHeight, null, 0, imageInWidth)
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

  def getImageFromBlack(row: Int, column: Int): BufferedImage = {
    getImageFrom(black, row, column)
  }

  def getImageFromTiers(row: Int, column: Int): BufferedImage = {
    getImageFrom(levels, row, column)
  }

}
