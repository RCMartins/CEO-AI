package ceo.image

import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO

import ceo.play._

import scala.collection.mutable
import scala.util.Try

object ImageLoader {

  val UNKNOWN_PIECE = "?"

  val backgroundImages: mutable.Set[(PieceImage, Option[PlayerTeam], Boolean)] = mutable.HashSet.empty
  val allPieceImages: mutable.Map[PieceImage, (String /* piece name */ , String /* file of the image */ )] = mutable.HashMap.empty
  val pieceImagesByName: mutable.Map[
    String /* OfficialName_Team */ ,
    mutable.HashSet[PieceImage] /* Set of possible images */ ] = mutable.HashMap.empty
  val allBoardImageData: mutable.Map[String, BoardImageData] = mutable.HashMap.empty

  val imagesToConfirm: mutable.Queue[(PieceImage, PieceData)] = mutable.Queue.empty
  val imagesUnknown: mutable.Queue[(PieceImage, Boolean)] = mutable.Queue.empty

  var whiteSquare: BufferedImage = null
  var blackSquare: BufferedImage = null

  def main(args: Array[String]): Unit = {
    initialize()
    //    getPiecesFromUnknownBoard(new File("Images/challenges/challenge-2018-01-11.png"), showPieces = true)
    //    containsPieceTest()
  }

  def initialize(): Unit = {
    if (backgroundImages.isEmpty) {
      println("Loading image knowledge...")
      val time = System.currentTimeMillis()
      DataLoader.loadPieceFiles()
      loadBackgroundSpecials(new File("Images/data/post/special.ceo"))
      loadKnownPieceInformation(BoardImageData.pieceImagesFolder)
      updatePieceImagesFromFiles(new File("Images/clean"), imageCutted = false)
      updatePieceImagesFromFiles(new File("Images/clean/cut"), imageCutted = true)
      println("Total loading time: " + (System.currentTimeMillis() - time))
    }
  }

  def loadBackgroundSpecials(file: File): Unit = {
    val (state, _) = DataLoader.initialize(file, showErrors = false)

    val imageLoader =
      new CuttedImageBoardLoader(new File(file.getAbsolutePath.stripSuffix("ceo") + "png"))

    for (row <- 0 until 8; column <- 0 until 8) {
      def currentImage = imageLoader.getImageAt(row, column)

      state.board(row, column) match {
        case Some(piece) if piece.data.name == "i" =>
        //          backgroundImagesToIgnore += currentImage
        case Some(piece) if piece.data.name == "1" =>
          backgroundImages += ((currentImage, Some(PlayerTeam.White), true))
        case Some(piece) if piece.data.name == "2" =>
          backgroundImages += ((currentImage, Some(PlayerTeam.White), false))
        case Some(piece) if piece.data.name == "3" =>
          backgroundImages += ((currentImage, Some(PlayerTeam.Black), true))
        case Some(piece) if piece.data.name == "4" =>
          backgroundImages += ((currentImage, Some(PlayerTeam.Black), false))
        case Some(piece) if piece.data.name == "u" => //ignore
        case Some(piece) if piece.data.name == "w" => //ignore
          backgroundImages += ((currentImage, None, true))
          pieceImagesByName.get("e") match {
            case None => pieceImagesByName += "e" -> mutable.HashSet(currentImage)
            case Some(set) => set += currentImage
          }
        case Some(piece) if piece.data.name == "b" => //ignore
          backgroundImages += ((currentImage, None, false))
          pieceImagesByName.get("e") match {
            case None => pieceImagesByName += "e" -> mutable.HashSet(currentImage)
            case Some(set) => set += currentImage
          }
        case _ =>
      }
    }

    whiteSquare = backgroundImages.find(p => p._2.isEmpty && p._3).head._1.bufferedImage
    blackSquare = backgroundImages.find(p => p._2.isEmpty && !p._3).head._1.bufferedImage
  }

  //  def loadKnownPieceInformation(file: File): Unit = {
  //    if (file.isDirectory) {
  //      file.listFiles().filter(file => file.isFile && file.getName.endsWith(".ceo")).foreach(loadKnownPieceInformation)
  //    } else {
  //      val imageFileName = file.getAbsolutePath.stripSuffix("ceo") + "png"
  //      val imageLoader = new CuttedImageBoardLoader(new File(imageFileName))
  //
  //      val pieceName = file.getName.stripSuffix(".ceo")
  //
  //      DataLoader.clearPiecesToCheck()
  //      val (state, _) = DataLoader.initialize(file, showErrors = false)
  //
  //      val boardImageData = new BoardImageData(pieceName)
  //
  //      for (row <- 0 until 8; column <- 0 until 8) {
  //        state.board(row, column) match {
  //          case Some(piece) if piece.data.name == "i" =>
  //            val image = imageLoader.getImageAt(row, column)
  //            boardImageData.loadImageFromData(BoardPos(row, column), image)
  //
  //            val pieceNameTeam = boardImageData.getPieceNameAt(row, column)
  //            pieceImagesByName.get(pieceNameTeam) match {
  //              case None =>
  //                pieceImagesByName += pieceNameTeam -> mutable.HashSet(image)
  //                allPieceImages += image -> (pieceNameTeam, imageFileName)
  //              case Some(set) =>
  //                if (!set(image)) {
  //                  set += image
  //                  allPieceImages += image -> (pieceNameTeam, imageFileName)
  //                }
  //            }
  //          case _ =>
  //        }
  //      }
  //      boardImageData.setUpdated()
  //      allBoardImageData += pieceName -> boardImageData
  //    }
  //  }

  def loadKnownPieceInformation(file: File): Unit = {
    if (file.isDirectory) {
      file.listFiles().filter(file => file.isFile && file.getName.endsWith(".png")).foreach(loadKnownPieceInformation)
    } else {
      val imageFileName = file.getAbsolutePath
      val imageLoader = new CuttedImageBoardLoader(file)

      val pieceName = file.getName.stripSuffix(".png")

      val boardImageData = new BoardImageData(pieceName)

      for (row <- List(0, 7); column <- 0 until 8) {
        val image = imageLoader.getImageAt(row, column)
        if (!isAnEmptySquare(image, BoardImageData.isBackgroundWhite(row, column))) {
          boardImageData.loadImageFromData(BoardPos(row, column), image)

          val pieceNameTeam = boardImageData.getPieceNameAt(row, column)
          pieceImagesByName.get(pieceNameTeam) match {
            case None =>
              pieceImagesByName += pieceNameTeam -> mutable.HashSet(image)
              allPieceImages += image -> (pieceNameTeam, imageFileName)
            case Some(set) =>
              if (!set(image)) {
                set += image
                allPieceImages += image -> (pieceNameTeam, imageFileName)
              }
          }
        }
      }
      boardImageData.setUpdated()
      allBoardImageData += pieceName -> boardImageData
    }
  }

  def updatePieceImagesFromFiles(file: File, imageCutted: Boolean, goRecursiveFolders: Boolean = false): Unit = {
    if (file.isDirectory) {
      val allFilesSet = if (goRecursiveFolders) file.listFiles().toSet else file.listFiles().toSet.filter(_.isFile)
      val ceoFileNames = allFilesSet.map(_.getName).filter(_.endsWith(".png"))
      val filesWithImagescreen = allFilesSet.map(_.getName).filter(name => name.endsWith(".ceo") && ceoFileNames.contains(name.stripSuffix("ceo") + "png"))

      val allFiles = allFilesSet.map(file => file.getName -> file).toMap

      filesWithImagescreen.map(allFiles).foreach(file =>
        updatePieceImagesFromFiles(file, imageCutted = imageCutted, goRecursiveFolders = goRecursiveFolders))

      allBoardImageData.foreach { case (_, boardImageData) =>
        if (boardImageData.saveToFile())
          Thread.sleep(100)
      }
      allFiles.foreach { case (_, f) =>
        val folder = new File(f.getParentFile.getAbsoluteFile, "post")
        if (!folder.exists())
          folder.mkdir()
        f.renameTo(new File(folder, f.getName))
      }
    } else {
      DataLoader.clearPiecesToCheck()
      val (state, unknownPieces) = DataLoader.initialize(file, showErrors = false)
      if (unknownPieces.isEmpty) {
        println(file.getName + " -> OK")
      } else {
        println(file.getName + " -> NOT OK")
        Thread.sleep(50)
        System.err.println(unknownPieces)
      }

      val fileName = file.getName

      val imageLoader: SimpleImageLoader =
        if (imageCutted)
          new CuttedImageBoardLoader(new File(file.getAbsolutePath.stripSuffix("ceo") + "png"))
        else
          new ImageBoardLoader(new File(file.getAbsolutePath.stripSuffix("ceo") + "png"))

      for (row <- 0 until 8; column <- 0 until 8) {
        val maybeName: Option[(String, PieceData)] =
          state.board(row, column) match {
            case Some(piece) if piece.data.isUnknown =>
              None
            case Some(piece) if piece.data.name == "i" => //ignore
              None
            case Some(piece) if piece.data.name.length == 1 =>
              ???
            case Some(piece) =>
              Some(piece.data.toString, piece.data)
            case None =>
              None
          }

        maybeName.foreach { case (name, pieceData) =>
          val image = imageLoader.getImageAt(row, column)
          pieceImagesByName.get(name) match {
            case None =>
              pieceImagesByName += name -> mutable.HashSet(image)
              allPieceImages += image -> (name, fileName)
            case Some(set) =>
              if (!set(image)) {
                set += image
                allPieceImages += image -> (name, fileName)
              }
          }
          val simpleName = pieceData.simpleName
          val boardImageData =
            allBoardImageData.getOrElseUpdate(simpleName, {
              new BoardImageData(simpleName)
            })
          boardImageData.setImage(image, pieceData, BoardPos(row, column))
        }
      }
    }
  }

  def getTeamPlay(pieceImage: PieceImage, useEqualPixels: Boolean = true): (Option[PlayerTeam], Boolean, Double) = {
    val image = pieceImage.bufferedImage
    if (useEqualPixels) {
      val list =
        backgroundImages.toList
          .map { case (PieceImage(background), maybePlayer, isSquareWhite) => (ImageUtils.getNumberOfEqualPixels(background, image), maybePlayer, isSquareWhite) }
          .sortBy(-_._1)
      val best = list.head
      (best._2, best._3, best._1.toDouble / (59 * 59))
    } else {
      val list =
        backgroundImages.toList
          .map { case (PieceImage(background), maybePlayer, isSquareWhite) => (ImageUtils.getDifferencePercent(background, image), maybePlayer, isSquareWhite) }
          .sortBy(_._1)
      val best = list.head
      (best._2, best._3, best._1)
    }
  }

  case class ImageState(
    currentTurnTeam: PlayerTeam,
    imageLoader: ImageBoardLoader,
    pieces: List[List[String]],
    lastMoveCoordinates: List[BoardPos]
  )

  def getPiecesFromUnknownBoard(imageFile: File, showPieces: Boolean): Option[ImageState] = {
    getPiecesFromUnknownBoard(ImageIO.read(imageFile), showPieces)
  }

  def guessPieceName(pieceImage: PieceImage): (String, Double) = {
    allPieceImages.get(pieceImage) match {
      case Some((pieceName, _)) =>
        (pieceName, 0.0)
      case None =>
        val sorted =
          pieceImagesByName.mapValues {
            _.map {
              image =>
                val value = ImageUtils.getDifferencePercent(pieceImage.bufferedImage, image.bufferedImage)
                value
            }.min
          }.toList
            .sortBy(_._2)

        val (name, bestValue) = sorted.head
        if (bestValue == 0.0)
          (name, bestValue)
        else if (bestValue < 1)
          (name, bestValue)
        else {
          val result = getTeamPlay(pieceImage)
          val result2 = getTeamPlay(pieceImage, useEqualPixels = false)
          if (result._3 > 0.94 || result2._3 < 1.0)
            ("e", 0.0)
          else
            (UNKNOWN_PIECE, 0.0)
        }
    }
  }

  def guessPieceNameWithPossibilities(pieceImage: PieceImage, boardPos: BoardPos, possiblePieceNames: List[String]): List[(String, Double)] = {
    val background = if (BoardImageData.isBackgroundWhite(boardPos.row, boardPos.column)) whiteSquare else blackSquare
    val cleanImage = ImageUtils.clearImageBackground(pieceImage.bufferedImage, background)

    val result =
      possiblePieceNames.map(name => (name, pieceImagesByName(name))).map { case (name, images) =>
        (name, images.map {
          image =>
            val value = ImageUtils.getDifferencePercent(cleanImage, image.bufferedImage)
            value
        }.min)
      }.filter(_._2 <= 4.0)
    result
  }

  def getPiecesFromUnknownBoard(imageToProcess: BufferedImage, showPieces: Boolean = false): Option[ImageState] = {

    val imageLoader = new ImageBoardLoader(imageToProcess)

    if (!imageLoader.isValid) {
      None
    } else {
      val result = Array.ofDim[String](8, 8)

      var lastMoveCoordinates: List[BoardPos] = Nil

      for (row <- 0 until 8; column <- 0 until 8) {
        val currentSquare = imageLoader.getImageAt(row, column)

        val (teamPlay, _, _) = getTeamPlay(currentSquare)
        if (teamPlay.nonEmpty) {
          lastMoveCoordinates = BoardPos(row, column) :: lastMoveCoordinates
        }

        val (n, d) = guessPieceName(currentSquare)
        if (n == UNKNOWN_PIECE)
          addPossibleUnknownImage(currentSquare)

        //        if (d == 0.0)
        result(row)(column) = f"$n%s"
        //        else
        //          result(row)(column) = f"$n%s[$d%.1f]"
      }

      if (showPieces) {
        val columnLengths: Seq[Int] =
          for (column <- 0 until 8) yield (0 until 8).map(row => result(row)(column).length).max

        for (row <- 0 until 8;
             column <- 0 until 8) {
          if (column == 7)
            println(result(row)(column))
          else
            printf("%-" + columnLengths(column) + "s ", result(row)(column))
        }
      }

      Some(ImageState(imageLoader.currentTeam, imageLoader, result.map(_.toList).toList, lastMoveCoordinates))
    }
  }

  def pieceNamesPretty(pieceNames: List[List[String]]): String = {
    val sb = new StringBuilder()

    val columnLengths: Seq[Int] =
      for (column <- 0 until 8) yield (0 until 8).map(row => pieceNames(row)(column).length).max

    for (row <- 0 until 8; column <- 0 until 8) {
      if (column == 7)
        sb.append(pieceNames(row)(column) + "\n")
      else {
        val s = pieceNames(row)(column)
        sb.append(s)
        sb.append(" " * (columnLengths(column) - s.length + 1))
      }
    }
    sb.toString
  }

  def loadBoardFromPieceNames(pieceNames: List[List[String]]): Option[GameState] = {
    if (pieceNames.exists(_.exists(_.startsWith("?"))))
      None
    else
      Some(DataLoader.loadBoard(pieceNames.map(_.mkString(" "))))
  }

  def loadBoardFromPieceNamesNoFilter(pieceNames: List[List[String]]): Option[GameState] = {
    Some(DataLoader.loadBoard(pieceNames.map(_.mkString(" "))))
  }

  def addPossibleUnknownImage(pieceImage: PieceImage): Unit =
    if (getTeamPlay(pieceImage)._1.isEmpty) {
      guessPieceName(pieceImage) match {
        case (UNKNOWN_PIECE, _) =>
          ImageLoader.imagesUnknown.synchronized {
            if (!imagesUnknown.exists(_._1 == pieceImage))
              imagesUnknown.enqueue((pieceImage, getTeamPlay(pieceImage)._2))
          }
        case _ => // piece is known
      }
    }

  def getPieceFromName(pieceName: String, isInWhiteSquare: Boolean): Try[Option[PieceImage]] = {
    Try {
      val List(namePartFull, team) = pieceName.split("_").toList
      val namePart = namePartFull.takeWhile(_ != '+')
      val tier = namePartFull.count(_ == '+')
      allBoardImageData.get(namePart).flatMap(_.getImage(PlayerTeam.apply(team), isInWhiteSquare, tier))
    }
  }

  def addNewPieceName(pieceImage: PieceImage, pieceNameFull: String, inWhiteSquare: Boolean): Unit = {
    val List(namePartFull, team) = pieceNameFull.split("_").toList
    val simpleName = namePartFull.takeWhile(_ != '+')
    val tier = namePartFull.count(_ == '+')
    val boardImageData =
      allBoardImageData.getOrElseUpdate(simpleName, {
        new BoardImageData(simpleName)
      })
    boardImageData.setImage(pieceImage, PlayerTeam.apply(team), inWhiteSquare, tier)
    boardImageData.saveToFile()
  }

  //  def containsPieceTest(): Unit = {
  //    val allImages = allBoardImageData.flatMap(_._2.allImages)
  //
  //    val hasPixel: Array[Array[Boolean]] = Array.fill(59, 59)(true)
  //
  //    allImages.map(image => (image, getTeamPlay(image)._2)).foreach { case (pieceImage, inWhiteSquare) =>
  //      val image = pieceImage.bufferedImage
  //      val width = image.getWidth
  //      val height = image.getHeight
  //      val background = if (inWhiteSquare) whiteSquare else blackSquare
  //      for (y <- 0 until height; x <- 0 until width) {
  //        if (image.getRGB(x, y) == background.getRGB(x, y))
  //          hasPixel(x)(y) = false
  //      }
  //    }
  //
  //    println("Center pixels:")
  //
  //    {
  //      val imageOut = new BufferedImage(59, 59, BufferedImage.TYPE_4BYTE_ABGR)
  //      val imageOutPixels = new Array[Int](59 * 59)
  //      for (y <- 0 until 59; x <- 0 until 59) {
  //        if (hasPixel(x)(y))
  //          imageOutPixels(x + y * 59) = Int.MaxValue
  //      }
  //      imageOut.setRGB(0, 0, 59, 59, imageOutPixels, 0, 59)
  //      ImageUtils.writeImage(imageOut, "Images/testCenter.png")
  //    }
  //
  //  }

  def isAnEmptySquare(pieceImage: PieceImage, isInWhiteSquare: Boolean): Boolean = {
    val image = pieceImage.bufferedImage
    val background = if (isInWhiteSquare) whiteSquare else blackSquare
    (for (y <- 44 until 47; x <- 27 until 30) yield (x, y)).exists { case (x, y) =>
      image.getRGB(x, y) == background.getRGB(x, y)
    }
  }

}
