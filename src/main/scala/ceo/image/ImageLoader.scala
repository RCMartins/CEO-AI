package ceo.image

import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO

import ceo.play.{DataLoader, GameState}

import scala.collection.mutable

object ImageLoader {

  val UNKNOWN_PIECE = "?"

  private val imageHashes: mutable.Set[Long] = mutable.Set[Long]()
  private val pieceImagesByName: mutable.Map[String /* OfficialName_Team */ , List[BufferedImage] /* List of possible images */ ] =
    mutable.Map[String, List[BufferedImage]]()
  private var pieceImagesByNameList: List[(String, List[BufferedImage])] = Nil

  def initialize(): Unit = {
    loadAllKnownPieces(new File("PRINTS/data"))
    loadAllKnownPieces(new File("PRINTS/challenges"))
  }

  def loadAllKnownPieces(file: File): Unit = {
    if (file.isDirectory) {
      val allFilesSet = file.listFiles().toSet
      val ceoFileNames = allFilesSet.map(_.getName).filter(_.endsWith(".png"))
      val filesWithPrintScreen = allFilesSet.map(_.getName).filter(name => name.endsWith(".ceo") && ceoFileNames.contains(name.stripSuffix("ceo") + "png"))

      val allFiles = allFilesSet.map(file => file.getName -> file).toMap
      //      println(filesWithPrintScreen.toList.sorted.mkString("\n"))

      filesWithPrintScreen.map(allFiles).foreach(loadBoardKnownPieces)

      pieceImagesByNameList = pieceImagesByName.toList.sortBy(_._1)
    } else
      new RuntimeException(s"Not a directory: $file")
  }

  def loadBoardKnownPieces(file: File): Unit = {
    DataLoader.loadPieceFiles()
    DataLoader.clearPiecesToCheck()
    val (state, unknownPieces) = DataLoader.initialize(file, showErrors = false)
    if (unknownPieces.isEmpty) {
      println(file.getName + " -> OK")
    } else {
      println(file.getName + " -> NOT OK")
      Thread.sleep(50)
      System.err.println(unknownPieces)
    }

    //      println(state)

    val imageLoader =
      new ImageBoardLoader(new File(file.getAbsolutePath.stripSuffix("ceo") + "png"))

    for (row <- 0 until 8; column <- 0 until 8) {
      val currentSquare = imageLoader.getImageWithTiersAt(row, column)
      val hash = ImageUtils.hashBufferedImage(currentSquare)
      if (!imageHashes(hash)) {

        val maybeName =
          state.board(row, column) match {
            case Some(piece) if piece.data.isUnknown =>
              Some(piece.data.name)
            case Some(piece) =>
              Some(piece.data.toString)
            case _ =>
              Some("e")
          }

        maybeName.foreach { name =>
          imageHashes.update(hash, included = true)
          pieceImagesByName.get(name) match {
            case None =>
              pieceImagesByName += name -> List(currentSquare)
            case Some(list) =>
              pieceImagesByName += name -> (currentSquare :: list)
          }
        }
      } else {
        if (state.board(row, column).isDefined &&
          !pieceImagesByName.exists(_._2.exists(image => ImageUtils.getDifferencePercent(currentSquare, image) == 0))) {
          System.err.println("Repetition is giving wrong results! " + state.board(row, column))
        }
      }
    }
  }

  def getPiecesFromUnknownBoard(imageFile: File, showPieces: Boolean): (ImageBoardLoader, List[List[String]]) = {
    getPiecesFromUnknownBoard(ImageIO.read(imageFile), showPieces)
  }

  def getPiecesFromUnknownBoard(image: BufferedImage, showPieces: Boolean = false): (ImageBoardLoader, List[List[String]]) = {

    println(pieceImagesByName.values.map(_.length).sum)

    val imageLoader = new ImageBoardLoader(image)
    //    val empty = new BufferedImage(imageLoader.SIZE + 2, imageLoader.SIZE + 2, BufferedImage.TYPE_4BYTE_ABGR)

    val result = Array.ofDim[String](8, 8)

    for (row <- 0 until 8; column <- 0 until 8) {
      val currentSquare = imageLoader.getImageWithTiersAt(row, column)

      //      if (ImageUtils.getDifferencePercent(currentSquare, empty) < 0.1) {
      //        print("EMPTY ")
      //      }

      val sorted =
        pieceImagesByName.mapValues {
          _.map { image =>
            val value = ImageUtils.getDifferencePercent(currentSquare, image)
            value
          }.min
        }.toList
          .sortBy(_._2)

      val (n, d) = {
        val (name, bestValue) = sorted.head
        if (bestValue == 0)
          (name, bestValue)
        else if (bestValue < 1)
          (name, bestValue)
        else
          (UNKNOWN_PIECE, 0.0)
      }

      if (d == 0.0)
        result(row)(column) = f"$n%s"
      else
        result(row)(column) = f"$n%s[$d%.1f]"
    }

    if (showPieces) {
      val columnLengths: Seq[Int] =
        for (column <- 0 until 8) yield (0 until 8).map(row => result(row)(column).length).max

      for (row <- 0 until 8; column <- 0 until 8) {
        if (column == 7)
          println(result(row)(column))
        else
          printf("%-" + columnLengths(column) + "s ", result(row)(column))
      }
    }

    (imageLoader, result.map(_.toList).toList)
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
    if (pieceNames.exists(_.contains(UNKNOWN_PIECE)))
      None
    else
      Some(DataLoader.loadBoard(pieceNames.map(_.mkString(" "))))
  }
}
