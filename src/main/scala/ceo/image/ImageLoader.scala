package ceo.image

import java.awt.image.BufferedImage
import java.io.File
import java.nio.file.{CopyOption, Files, StandardCopyOption}
import javax.imageio.ImageIO

import ceo.play._

import scala.collection.mutable

object ImageLoader {

  val UNKNOWN_PIECE = "?"

  private val backgroundLastPlayWhite: mutable.Set[PieceImage] = mutable.HashSet.empty
  private val backgroundLastPlayBlack: mutable.Set[PieceImage] = mutable.HashSet.empty
  private val allPieceImages: mutable.Map[PieceImage, (String /* piece name */ , String /* file of the image */ )] = mutable.HashMap.empty
  private val pieceImagesByName: mutable.Map[
    String /* OfficialName_Team */ ,
    mutable.HashSet[PieceImage] /* Set of possible images */ ] = mutable.HashMap.empty
  private val allBoardImageData: mutable.Map[String, BoardImageData] = mutable.HashMap.empty

  def main(args: Array[String]): Unit = {
    initialize()
    getPiecesFromUnknownBoard(new File("PRINTS/challenges/challenge-2018-01-11.png"), showPieces = true)
  }

  def initialize(): Unit = {
    println("Loading image knowledge...")
    val time = System.currentTimeMillis()
    DataLoader.loadPieceFiles()
    loadBackgroundSpecials(new File("PRINTS/data/post/special.ceo"))
    loadKnownPieceInformation(new File("PRINTS/data/pieces"))
    updatePieceImagesFromFiles(new File("PRINTS/clean"), imageCutted = false)
    updatePieceImagesFromFiles(new File("PRINTS/clean/cut"), imageCutted = true)
    println("Total loading time: " + (System.currentTimeMillis() - time))
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
          backgroundLastPlayWhite += currentImage
        case Some(piece) if piece.data.name == "2" =>
          backgroundLastPlayBlack += currentImage
        case Some(piece) if piece.data.name == "u" => //ignore
        case None =>
          pieceImagesByName.get("e") match {
            case None => pieceImagesByName += "e" -> mutable.HashSet(currentImage)
            case Some(set) => set += currentImage
          }
        case _ =>
      }
    }
  }

  def loadKnownPieceInformation(file: File): Unit = {
    if (file.isDirectory) {
      file.listFiles().filter(file => file.isFile && file.getName.endsWith(".ceo")).foreach(loadKnownPieceInformation)
    } else {
      val imageFileName = file.getAbsolutePath.stripSuffix("ceo") + "png"
      val imageLoader = new CuttedImageBoardLoader(new File(imageFileName))

      val pieceName = file.getName.stripSuffix(".ceo")

      DataLoader.clearPiecesToCheck()
      val (state, _) = DataLoader.initialize(file, showErrors = false)

      val boardImageData = new BoardImageData(pieceName)

      for (row <- 0 until 8; column <- 0 until 8) {
        state.board(row, column) match {
          case Some(piece) if piece.data.name == "i" =>
            val image = imageLoader.getImageAt(row, column)
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
          case _ =>
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
      val filesWithPrintScreen = allFilesSet.map(_.getName).filter(name => name.endsWith(".ceo") && ceoFileNames.contains(name.stripSuffix("ceo") + "png"))

      val allFiles = allFilesSet.map(file => file.getName -> file).toMap

      filesWithPrintScreen.map(allFiles).foreach(file =>
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
          boardImageData.setImage(pieceData, BoardPos(row, column), image)
        }
      }
    }
  }

  def getTeamPlay(pieceImage: PieceImage): Option[PlayerTeam] = {
    val image = pieceImage.bufferedImage
    val list =
      (backgroundLastPlayWhite.toList
        .map(ignore => (ImageUtils.getNumberOfEqualPixels(ignore.bufferedImage, image), ignore, Some(PlayerTeam.White))) ++
        backgroundLastPlayBlack.toList
          .map(ignore => (ImageUtils.getNumberOfEqualPixels(ignore.bufferedImage, image), ignore, Some(PlayerTeam.Black))) ++
        pieceImagesByName("e").toList
          .map(ignore => (ImageUtils.getNumberOfEqualPixels(ignore.bufferedImage, image), ignore, None))
        )
        .sortBy(-_._1)
    val best = list.head
    val team: Option[PlayerTeam] = best._3

    team
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

  def getPiecesFromUnknownBoard(imageToProcess: BufferedImage, showPieces: Boolean = false): Option[ImageState] = {

    val imageLoader = new ImageBoardLoader(imageToProcess)

    if (!imageLoader.isValid) {
      None
    } else {
      val result = Array.ofDim[String](8, 8)

      var lastMoveCoordinates: List[BoardPos] = Nil

      for (row <- 0 until 8; column <- 0 until 8) {
        val currentSquare = imageLoader.getImageAt(row, column)

        val teamPlay = getTeamPlay(currentSquare)
        if (teamPlay.nonEmpty) {
          lastMoveCoordinates = BoardPos(row, column) :: lastMoveCoordinates
        }

        val (n, d) =
          allPieceImages.get(currentSquare) match {
            case Some((pieceName, fileName)) =>
              (pieceName, 0.0)
            case None =>
              val sorted =
                pieceImagesByName.mapValues {
                  _.map {
                    image =>
                      val value = ImageUtils.getDifferencePercent(currentSquare.bufferedImage, image.bufferedImage)
                      value
                  }.min
                }.toList
                  .sortBy(_._2)

              val (name, bestValue) = sorted.head
              if (bestValue == 0.0)
                (name, bestValue)
              else if (bestValue < 1)
                (name, bestValue)
              else
                (UNKNOWN_PIECE, 0.0)
          }

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
}
