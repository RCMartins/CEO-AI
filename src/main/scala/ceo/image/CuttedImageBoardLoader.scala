package ceo.image

import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO

import scala.util.Try

class CuttedImageBoardLoader(val imageIn: BufferedImage) extends SimpleImageLoader{

  def this(imageFile: File) = {
    this {
      val image = Try(ImageIO.read(imageFile))
    if (image.isFailure)
      println("Error reading image!")
      image.get
    }
  }

  val SIZE: Int = 59
  val cornerX = 1
  val cornerY = 1

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
    val imageInWidth = imageIn.getWidth
    val imageInPixels: Array[Int] = imageIn.getRGB(0, 0, imageInWidth, imageIn.getHeight, null, 0, imageInWidth)
    val imageOut = new BufferedImage(SIZE, SIZE, BufferedImage.TYPE_4BYTE_ABGR)
    val imageOutPixels = new Array[Int](SIZE * SIZE)
    val ix = square.left
    val iy = square.top
    for (y <- iy until square.bottom; x <- ix until square.right) {
      val kIn = x + y * imageInWidth
      val kOut = (x - ix) + (y - iy) * SIZE
      imageOutPixels(kOut) = imageInPixels(kIn)
    }
    imageOut.setRGB(0, 0, SIZE, SIZE, imageOutPixels, 0, SIZE)
    imageOut
  }

}
