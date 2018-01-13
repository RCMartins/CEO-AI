package ceo.image

import java.awt.image.BufferedImage
import java.awt.{Image, Rectangle, Robot, Toolkit}
import java.io.{File, IOException}
import javax.imageio.ImageIO

object ImageUtils {

  def saveCurrentImagescreen: BufferedImage = {
    val toolkit = Toolkit.getDefaultToolkit
    val screenRect = new Rectangle(toolkit.getScreenSize)
    val robot = new Robot
    val image = robot.createScreenCapture(screenRect)
    val filePath = "Images/print%d.png".format {
      System.currentTimeMillis()
    }
    writeImage(image, filePath)
    image
  }

  def writeImage(image: BufferedImage, filePath: String): Unit = {
    try
      ImageIO.write(image, "png", new File(filePath))
    catch {
      case e: IOException =>
        println("Error: " + e.getMessage)
        Thread.sleep(50)
        println("Trying again...")
        writeImage(image, filePath)
    }
  }

  def writeImage(image: BufferedImage, file: File): Unit = {
    try
      ImageIO.write(image, "png", file)
    catch {
      case e: IOException =>
        println("Error: " + e.getMessage)
        Thread.sleep(50)
        println("Trying again...")
        writeImage(image, file)
    }
  }

  /**
    * Converts a given Image into a BufferedImage
    *
    * @param img
    * The Image to be converted
    * @return The converted BufferedImage
    */
  def toBufferedImage(img: Image): BufferedImage = {
    img match {
      case image: BufferedImage =>
        image
      case _ =>
        // Create a buffered image with transparency
        val bImage = new BufferedImage(img.getWidth(null), img.getHeight(null), BufferedImage.TYPE_INT_ARGB)
        // Draw the image on to the buffered image
        val bGr = bImage.createGraphics
        bGr.drawImage(img, 0, 0, null)
        bGr.dispose()
        // Return the buffered image
        bImage
    }
  }

  def getDifferencePercent(img1: BufferedImage, img2: BufferedImage): Double = {
    val width = img1.getWidth
    val height = img1.getHeight
    val width2 = img2.getWidth
    val height2 = img2.getHeight
    if (width > width2 || height > height2)
      throw new IllegalArgumentException("Images must have the same dimensions: (%d,%d) vs. (%d,%d)".format(width, height, width2, height2))
    var diff = 0
    for (y <- 0 until height; x <- 0 until width) {
      diff += pixelDiff(img1.getRGB(x, y), img2.getRGB(x, y))
    }
    val maxDiff = 3L * 255 * width * height
    100.0 * diff / maxDiff
  }

  def getNumberOfEqualPixels(img1: BufferedImage, img2: BufferedImage): Int = {
    val width = img1.getWidth
    val height = img1.getHeight
    val width2 = img2.getWidth
    val height2 = img2.getHeight
    if (width > width2 || height > height2)
      throw new IllegalArgumentException("Images must have the same dimensions: (%d,%d) vs. (%d,%d)".format(width, height, width2, height2))
    var count = 0
    for (y <- 0 until height; x <- 0 until width) {
      if (img1.getRGB(x, y) == img2.getRGB(x, y))
        count += 1
    }
    count
  }

  def getNumberOfDifferentPixels(img1: BufferedImage, img2: BufferedImage): Int = {
    val width = img1.getWidth
    val height = img1.getHeight
    val width2 = img2.getWidth
    val height2 = img2.getHeight
    if (width > width2 || height > height2)
      throw new IllegalArgumentException("Images must have the same dimensions: (%d,%d) vs. (%d,%d)".format(width, height, width2, height2))
    var count = 0
    for (y <- 0 until height; x <- 0 until width) {
      if (img1.getRGB(x, y) != img2.getRGB(x, y))
        count += 1
    }
    count
  }

  private def pixelDiff(rgb1: Int, rgb2: Int) = {
    val a1 = (rgb1 >> 24) & 0xff
    val r1 = (rgb1 >> 16) & 0xff
    val g1 = (rgb1 >> 8) & 0xff
    val b1 = rgb1 & 0xff
    val a2 = (rgb2 >> 24) & 0xff
    val r2 = (rgb2 >> 16) & 0xff
    val g2 = (rgb2 >> 8) & 0xff
    val b2 = rgb2 & 0xff
    Math.abs(a1 - a2) + Math.abs(r1 - r2) + Math.abs(g1 - g2) + Math.abs(b1 - b2)
  }

  def hashBufferedImage(img: BufferedImage): Int = {
    val width = img.getWidth
    val height = img.getHeight
    var hash: Int = 7
    for (y <- 0 until height; x <- 0 until width) {
      val index = y * 8 + x

      val pixelColor = img.getRGB(x, y)
      hash = 31 * hash + ((pixelColor >> 24) & 0xff + 1) * index
      hash = 31 * hash + ((pixelColor >> 16) & 0xff + 1) * index
      hash = 31 * hash + ((pixelColor >> 8) & 0xff + 1) * index
      hash = 31 * hash + (pixelColor & 0xff + 1) * index
    }
    hash
  }

  def joinBufferedImages(imageUnder: BufferedImage, imageOver: BufferedImage): BufferedImage = {
    val sqWidth = imageUnder.getWidth
    val sqHeight = imageUnder.getHeight
    val imageUnderPixels: Array[Int] = imageUnder.getRGB(0, 0, sqWidth, sqHeight, null, 0, sqWidth)
    val imageOverPixels: Array[Int] = imageOver.getRGB(0, 0, sqWidth, sqHeight, null, 0, sqWidth)
    val imageOut = new BufferedImage(sqWidth, sqHeight, BufferedImage.TYPE_4BYTE_ABGR)
    val imageOutPixels = new Array[Int](sqWidth * sqHeight)
    for (y <- 0 until imageUnder.getHeight; x <- 0 until imageUnder.getWidth) {
      val index = x + y * sqWidth
      if (imageUnderPixels(index) == 0)
        imageOutPixels(index) = imageOverPixels(index)
      else
        imageOutPixels(index) = imageUnderPixels(index)
    }
    imageOut.setRGB(0, 0, sqWidth, sqHeight, imageOutPixels, 0, sqWidth)
    imageOut
  }

  def clearImageBackground(image: BufferedImage, background: BufferedImage): BufferedImage = {
    val sqWidth = image.getWidth
    val sqHeight = image.getHeight
    val imagePixels: Array[Int] = image.getRGB(0, 0, sqWidth, sqHeight, null, 0, sqWidth)
    val backgroundPixels: Array[Int] = background.getRGB(0, 0, sqWidth, sqHeight, null, 0, sqWidth)
    val imageOut = new BufferedImage(sqWidth, sqHeight, BufferedImage.TYPE_4BYTE_ABGR)
    val imageOutPixels = new Array[Int](sqWidth * sqHeight)
    for (y <- 0 until sqHeight; x <- 0 until sqWidth) {
      val index = x + y * sqWidth
      if (imagePixels(index) == backgroundPixels(index))
        imageOutPixels(index) = 0
      else
        imageOutPixels(index) = imagePixels(index)
    }
    imageOut.setRGB(0, 0, sqWidth, sqHeight, imageOutPixels, 0, sqWidth)
    imageOut
  }
}
