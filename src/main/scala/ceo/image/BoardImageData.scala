package ceo.image

import java.awt.Graphics2D
import java.awt.image.BufferedImage
import java.io.{BufferedWriter, File, FileWriter}
import javax.imageio.ImageIO

import ceo.image.BoardImageData.isBackgroundWhite
import ceo.play.PlayerTeam.{Black, White}
import ceo.play.{BoardPos, PieceData, PlayerTeam}

object BoardImageData {
  val pieceImagesFolder: File = new File("Data/piece-images")

  def isBackgroundWhite(row: Int, column: Int): Boolean = if (row % 2 == 0) column % 2 == 0 else column % 2 == 1
}

class BoardImageData(pieceName: String) {

  def getPieceNameAt(row: Int, column: Int): String = {
    val tier = column / 2
    s"$pieceName${"+" * tier}_${if (row < 4) "Black" else "White"}"
  }

  val file: File = new File(BoardImageData.pieceImagesFolder, s"$pieceName.png")

  private val positions: Array[Array[(Option[PieceImage])]] = Array.fill(8, 8)(None)
  private var _hasNewInformation = false

  def allImages: List[PieceImage] = positions.flatMap(_.flatMap(_.toSeq)).toList

  def getImage(team: PlayerTeam, whiteSquare: Boolean, tier: Int): Option[PieceImage] = {
    require(tier >= 0 && tier <= 3)
    val (rowImage, columnImage) = (team, whiteSquare) match {
      case (Black, true) => (0, tier * 2)
      case (Black, false) => (0, tier * 2 + 1)
      case (White, true) => (7, tier * 2 + 1)
      case (White, false) => (7, tier * 2)
    }
    positions(rowImage)(columnImage)
  }

  def getImage(pieceData: PieceData, boardPos: BoardPos): Option[PieceImage] = {
    val inWhiteSquare = if (boardPos.row % 2 == 0) boardPos.column % 2 == 0 else boardPos.column % 2 == 1
    getImage(pieceData, inWhiteSquare)
  }

  def getImage(pieceData: PieceData, inWhiteSquare: Boolean): Option[PieceImage] = {
    val (rowImage, columnImage) = (pieceData.team, inWhiteSquare) match {
      case (Black, true) => (0, pieceData.tier * 2)
      case (Black, false) => (0, pieceData.tier * 2 + 1)
      case (White, true) => (7, pieceData.tier * 2 + 1)
      case (White, false) => (7, pieceData.tier * 2)
    }
    positions(rowImage)(columnImage)
  }

  def setImage(pieceImage: PieceImage, team: PlayerTeam, whiteSquare: Boolean, tier: Int): Unit = {
    require(tier >= 0 && tier <= 3)
    val (rowImage, columnImage) = (team, whiteSquare) match {
      case (Black, true) => (0, tier * 2)
      case (Black, false) => (0, tier * 2 + 1)
      case (White, true) => (7, tier * 2 + 1)
      case (White, false) => (7, tier * 2)
    }
    if (positions(rowImage)(columnImage).isEmpty) {
      positions(rowImage)(columnImage) = Some(pieceImage)
      _hasNewInformation = true
    }
  }

  def setImage(pieceImage: PieceImage, pieceData: PieceData, boardPos: BoardPos): Unit = {

    val (rowImage, columnImage) = (pieceData.team, isBackgroundWhite(boardPos.row, boardPos.column)) match {
      case (Black, true) => (0, pieceData.tier * 2)
      case (Black, false) => (0, pieceData.tier * 2 + 1)
      case (White, true) => (7, pieceData.tier * 2 + 1)
      case (White, false) => (7, pieceData.tier * 2)
    }
    if (positions(rowImage)(columnImage).isEmpty) {
      positions(rowImage)(columnImage) = Some(pieceImage)
      _hasNewInformation = true
    }
  }

  def loadImageFromData(boardPos: BoardPos, pieceImage: PieceImage): Unit = {
    positions(boardPos.row)(boardPos.column) = Some(pieceImage)
  }

  def hasNewInformation: Boolean = _hasNewInformation

  def setUpdated(): Unit = _hasNewInformation = false

  def saveToFile(): Boolean = {
    if (_hasNewInformation) {
      ImageUtils.writeImage(toImage, file)

      _hasNewInformation = false
      true
    } else
      false
  }

  private def toImage: BufferedImage = {
    val emptyLoader = new CuttedImageBoardLoader(ImageIO.read(new File("Images/data/empty.png")))
    val fullImageSize = emptyLoader.imageIn.getWidth

    import java.awt.image.BufferedImage
    val imageOut = new BufferedImage(fullImageSize, fullImageSize, BufferedImage.TYPE_4BYTE_ABGR)
    val g2: Graphics2D = imageOut.getGraphics.asInstanceOf[Graphics2D]
    g2.drawImage(emptyLoader.imageIn, 0, 0, null)

    for (row <- 0 until 8; column <- 0 until 8) {
      positions(row)(column) match {
        case None =>
        case Some(PieceImage(imageIn)) =>
          val square = emptyLoader.getSquare(row, column)
          val ix = square.left
          val iy = square.top
          g2.drawImage(imageIn, ix, iy, null)
      }
    }

    g2.dispose()
    imageOut
  }

  private def toCEOString: String = {
    positions.map(_.map(_.map(_ => "i").getOrElse("u")).mkString(" ")).mkString("\n")
  }
}
