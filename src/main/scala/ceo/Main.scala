package ceo

import java.awt._
import java.awt.image.BufferedImage
import java.io.{File, IOException}
import java.nio.file.Files
import java.nio.file.StandardCopyOption.REPLACE_EXISTING
import javax.imageio.ImageIO

import scala.collection.mutable

object Main {

  private val LIMIT_TO_MAKE_BLACK: Int = 70
  private var squares: Array[Array[Boolean]] = _
  var SIZE: Int = _
  private var COUNT: Int = 0
  private var COUNT_UNKNOWN: Int = 0
  //  private var thread: Thread = _
  private val knownPieces: mutable.Map[String, Piece] = mutable.Map[String, Piece]()
  private val field: mutable.Map[Coordinate, Piece] = mutable.Map[Coordinate, Piece]()
  private val fieldAmount: Array[Array[Array[Int]]] = Array.ofDim[Int](9, 9, 4)
  private val fieldLevel: Array[Array[Int]] = Array.ofDim[Int](9, 9)

  def main(args: Array[String]): Unit = {
    FillKnownPieces.reload()
    loadKnownPieces()

    val initTime: Long = System.currentTimeMillis()

    //			BufferedImage imageIn = printScreenCurrent();

    val imageIn: BufferedImage =
      ImageIO.read(new File("PRINTS/print_test7.png"))

    val black: BufferedImage = findSquareBorders(imageIn)
    if (SIZE == 0) {
      println("Invalid Image")
    } else {
      for (y <- 1 to 8; x <- 1 to 8) {
        val piece: Piece = getPieceFromBlack(black, x, y)
        field.put(Coordinate(x, y), piece)
        if (piece != null && piece != Piece.BLANK_SQUARE && piece.pieceType.isEmpty) {
          //			String str = "UnknownPieces/" + UUID.randomUUID().toString() + ".png";
          val str: String = "UnknownPieces/%02d.png".format {
            COUNT_UNKNOWN += 1
            COUNT_UNKNOWN - 1
          }
          copyImage("SQUARES/" + x + "-" + y + ".png", str)
        }
      }
    }
    val total: Long = System.currentTimeMillis() - initTime
    println("Time: " + total)
    Toolkit.getDefaultToolkit.beep()
  }

  def copyImage(sourcePath: String, targetPath: String): Unit = {
    val source: File = new File(sourcePath)
    val target: File = new File(targetPath)
    Files.copy(source.toPath, target.toPath, REPLACE_EXISTING)
  }

  private def loadKnownPieces(): Unit = {
    val folder: File = new File("KnownPieces")

    for (file <- folder.listFiles) {
      if (file.isFile) {
        val imageIn: BufferedImage = ImageIO.read(file)
        val imageInPixels: Array[Int] = imageIn.getRGB(0, 0, imageIn.getWidth, imageIn.getHeight, null, 0, imageIn.getWidth)
        val piece: Piece = buildPiece(imageInPixels, imageIn.getWidth, -1, -1)
        knownPieces.get(piece.pieceType) match {
          case None =>
            knownPieces(file.getName) = piece
          case Some(oldPiece) =>
            System.out.println("Duplicate Known Pieces: " + file.getName + " and " + oldPiece.pieceType)
        }
      } else if (file.isDirectory) {
        //				System.out.println("Directory " + listOfFiles[i].getName());
      }
    }
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

    System.out.println(CORNER_X + " " + CORNER_Y)
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

  private def getSquare(i: Int, j: Int) = {
    val x = Square.INIT_X + (i - 1) * (SIZE - 1)
    val y = Square.INIT_Y + (j - 1) * (SIZE - 1)
    new Square(x, y, x + SIZE - 1, y + SIZE - 1)
  }

  def getPieceFromBlack(imageIn: BufferedImage, i: Int, j: Int): Piece = {
    val square = getSquare(i, j)
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
    val piece = buildPiece(imageOutPixels, sqWidth, i, j)
    writeImage(imageOut, "SQUARES/" + i + "-" + j + ".png")
    piece
  }

  //	private static BufferedImage getFullImageInBlack(BufferedImage imageIn) {
  //		int imageInWidth = imageIn.getWidth();
  //		int imageInHeight = imageIn.getWidth();
  //		int[] imageInPixels = imageIn.getRGB(0, 0, imageInWidth, imageInHeight, null, 0, imageInWidth);
  //	}

  private def buildPiece(imageOutPixels: Array[Int], sqWidth: Int, x: Int, y: Int): Piece = {
    val black = new Array[Boolean](imageOutPixels.length)
    var blackCount = 0
    for (i <- imageOutPixels.indices) {
      //			int n = imageOutPixels[i];
      //			String h = String.format("0x%8s", Integer.toHexString(n)).replace(' ', '0');
      //			System.out.print(h + " ");
      if (imageOutPixels(i) == 0) black(i) = false //System.out.print("  ");
      else {
        black(i) = true //System.out.print("# ");

        blackCount += 1
      }
      //			if ((i + 1) % sqWidth == 0)
      //				System.out.println();
    }
    if (blackCount > 0) {
      if (knownPieces == null) return null
      var trueCount = 0
      val piece = new Piece(black)
      for ((key, value) <- knownPieces) {
        val knownPiece = value
        if (knownPiece.comparePerc(piece, 0.9)) {
          piece.pieceType = key
          piece.setWhite(key.contains("white"))
          trueCount += 1
        }
      }
      if (trueCount > 1) System.out.println("Multiple matches!: " + x + " " + y)
      piece
    }
    else Piece.BLANK_SQUARE
  }

  def findSquareBorders(imageIn: BufferedImage): BufferedImage = {
    val width = imageIn.getWidth
    val height = imageIn.getHeight
    val imageInPixels = imageIn.getRGB(0, 0, width, height, null, 0, width)

    {
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
      writeImage(imageOut, "PRINTS/temp/cornerCheck.png")
    }

    updateSquareSize(imageIn)
    val result: BufferedImage = {
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
      writeImage(imageOut, "PRINTS/temp/black.png")
      imageOut
    }

    {
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
      writeImage(imageOut, "PRINTS/temp/special.png")
    }
    result
  }

  private def printScreenCurrent: BufferedImage = {
    val toolkit = Toolkit.getDefaultToolkit
    val screenRect = new Rectangle(toolkit.getScreenSize)
    var robot: Robot = null
    try
      robot = new Robot
    catch {
      case e: AWTException =>
        e.printStackTrace()
        return null
    }
    val image = robot.createScreenCapture(screenRect)
    val filePath = "PRINTS/print%03d.png".format {
      COUNT += 1
      COUNT - 1
    }
    writeImage(image, filePath)
    image
  }

  private def writeImage(image: BufferedImage, filePath: String): Unit = {
    try
      ImageIO.write(image, "png", new File(filePath))
    catch {
      case e: IOException =>
        e.printStackTrace()
    }
  }

  //	private static int[] checkClipboardImage(BufferedImage bufferedImage) {
  //		//			final long currentTimeMillis = System.currentTimeMillis();
  //		//			final BufferedImage bufferedImage = toBufferedImage(getImageFromClipboard());
  //		//			if (bufferedImage != null) {
  //		//				int[] pixels = ((DataBufferInt) bufferedImage.getRaster().getDataBuffer()).getData();
  //		//
  //		//				checkPixels(pixels, bufferedImage.getWidth());
  //		//			}
  //		//			final long time = System.currentTimeMillis() - currentTimeMillis;
  //		//			System.out.println("Check image time: " + time + "ms");
  //
  //		int[] pixels = ((DataBufferInt) bufferedImage.getRaster().getDataBuffer()).getData();
  //		return pixels;


  //	private static void checkPixels(int[] pixels, int width) {
  //		for (int i = 0; i < JOIN_PIXELS.length; i++) {
  //			final int ix = JOIN_PIXELS[i][0] + OFFSETX;
  //			final int iy = JOIN_PIXELS[i][1] + OFFSETY;
  //			median[i] = 0;
  //			for (int y = iy; y < iy + SIZEY; y++) {
  //				for (int x = ix; x < ix + SIZEX; x++) {
  //					Color c = new Color((pixels[x + y * width]));
  //					int red = c.getRed();
  //					int green = c.getGreen();
  //					int blue = c.getBlue();

  //					int value = red;

  //					//					System.out.printf("%3d, ", value);
  //					median[i] = Math.max(median[i], value);
  //				}
  //				//				System.out.println();
  //			}
  //			//			System.out.println();
  //			//median[i] /= (SIZEX * SIZEY);
  //		}

  //		System.out.println();
  //		for (int i = 0; i < median.length; i++) {
  //			System.out.printf("%6.2f ", median[i]);
  //			if (i == 4)
  //				System.out.println();


  //	/**
  //	 * Get an image off the system clipboard.
  //	 *
  //	 * @return Returns an Image if successful; otherwise returns null.
  //	 */
  //	public static Image getImageFromClipboard() {
  //		Transferable transferable = Toolkit.getDefaultToolkit().getSystemClipboard().getContents(null);
  //		if (transferable != null && transferable.isDataFlavorSupported(DataFlavor.imageFlavor)) {
  //			try {
  //				return (Image) transferable.getTransferData(DataFlavor.imageFlavor);
  //			} catch (UnsupportedFlavorException e) {
  //				// handle this as desired
  //				e.printStackTrace();
  //			} catch (IOException e) {


  //		} else {
  //			System.err.println("getImageFromClipboard: That wasn't an image!");

  //		return null;


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
        val bimage = new BufferedImage(img.getWidth(null), img.getHeight(null), BufferedImage.TYPE_INT_ARGB)
        // Draw the image on to the buffered image
        val bGr = bimage.createGraphics
        bGr.drawImage(img, 0, 0, null)
        bGr.dispose()
        // Return the buffered image
        bimage
    }
  }

}
