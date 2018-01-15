package ceo.control

import java.awt.Rectangle
import java.io.{BufferedWriter, File, FileWriter}
import java.text.SimpleDateFormat
import java.util.Date

import ceo.image.ImageLoader.ImageState
import ceo.image.{ImageBoardLoader, ImageLoader, ImageUtils, Square}
import ceo.menu.Exceptions.BoardStartsWithUnknownPieces
import ceo.play.{BoardPos, _}

object MainControl {

  private val autoScreenFolder = new File("Images/auto-screen")

  val minX = 2220
  val minY = 312
  val sizeX = 1000
  val sizeY = 650

  // private val strategy = Strategy.AlphaBetaPruning(5)
  private val strategy = Strategy.AlphaBetaPruningIterativeDeepening

  def main(args: Array[String]): Unit = {
    start()
  }

  def start(): Unit = {
    ImageLoader.initialize()

    val dateFormatter = new SimpleDateFormat("yyyy-MM-dd-HH-mm-ss")
    val submittedDateConvert = new Date()
    val name = dateFormatter.format(submittedDateConvert)
    val gameFolder = new File(autoScreenFolder, name)

    playTurn(gameFolder, None, false)
  }

  def playTurn(gameFolder: File, gameState: Option[GameState], lastImageHadProblems: Boolean): Unit = {
    if (gameState.exists(_.winner != PlayerWinType.NotFinished)) {
      println("Game Over!")
      println("Result: " + gameState.get.winner)
    } else {
      val screen = MouseControl.robot.createScreenCapture(new Rectangle(minX, minY, sizeX, sizeY))

      ImageLoader.getPiecesFromUnknownBoard(screen) match {
        case None =>
          println("Board not found!")
        case Some(ImageState(PlayerTeam.Black, _, _, _)) =>
          println("Still in Black turn, waiting...")
          // TODO use this information to detect errors in state update
          Thread.sleep(500)
          playTurn(gameFolder, gameState, false)
        case Some(ImageState(PlayerTeam.White, loader, pieceNames, lastMoveCoordinates)) =>
          val stateOption = ImageLoader.loadBoardFromPieceNamesNoFilter(pieceNames)
          if (gameState.isEmpty && stateOption.nonEmpty) {
            // TODO: check that we have images for all the pieces / white and black squares / possible evolutions / charm / etc
            val unknownPieces = stateOption.get.allPieces.filter(_.data.isUnknown)
            if (unknownPieces.nonEmpty) {
              println(unknownPieces.mkString("\n"))
              throw new BoardStartsWithUnknownPieces(unknownPieces)
            } else {
              val missingImages =
                stateOption.get.allPieces.map(_.data).flatMap { data =>
                  ImageLoader.allBoardImageData.get(data.simpleName) match {
                    case None =>
                      List(s"Missing all images from '${data.simpleName}'")
                    case Some(boardImageData) =>
                      val missingWhite = boardImageData.getImage(data, inWhiteSquare = true).isEmpty
                      val missingBlack = boardImageData.getImage(data, inWhiteSquare = false).isEmpty
                      if (missingWhite && missingBlack)
                        List(s"Missing image '${data.name}'")
                      else if (missingWhite)
                        List(s"Missing image '${data.name}' in a white square")
                      else if (missingBlack)
                        List(s"Missing image '${data.name}' in a black square")
                      else
                        Nil
                  }
                }
              if (missingImages.nonEmpty) {
                println("Possible missing images necessary to play:")
                println(missingImages.mkString("\n"))
              } else {
                println("All piece images necessary to start game, there can be promotions / charms / summons that are missing...")
              }
            }
          }

          val dateFormatter = new SimpleDateFormat("yyyy-MM-dd-HH-mm-ss")
          val submittedDateConvert = new Date()
          val name = dateFormatter.format(submittedDateConvert)
          gameFolder.mkdirs()
          ImageUtils.writeImage(screen, new File(gameFolder, s"$name.png"))

          val checkEnemy =
            checkEnemyPlay(gameState, stateOption, lastMoveCoordinates, loader)

          if (gameState.isEmpty || checkEnemy.exists(_.allPieces.forall(!_.data.isUnknown))) {
            val startingState = checkEnemy.getOrElse(stateOption.get)
            println(startingState.getBoardPieceNames)
            if (startingState.winner != PlayerWinType.NotFinished) {
              println("Game Over!")
              println("Result: " + startingState.winner)
            } else {

              val time = System.currentTimeMillis()
              val Some(stateAfter) = strategy.chooseMove(startingState)
              println(s"Turn calc time: ${System.currentTimeMillis() - time}")
              val move = stateAfter.movesHistory.head

              println("Move done: " + move)
              println("Game should look like this:")
              println(stateAfter)

              val controllerMove = move.getControllerMove
              val (pos1, pos2) = controllerMove
              println(controllerMove)

              val sq1 = loader.getSquare(pos1.row, pos1.column)
              val sq2 = loader.getSquare(pos2.row, pos2.column)

              def toScreenX(sq: Square): Int = sq.getCenterX.toInt

              def toScreenY(sq: Square): Int = sq.getCenterY.toInt

              val (beforeX, beforeY) = MouseControl.getMousePosition
              MouseControl.moveMouse(minX + toScreenX(sq1), minY + toScreenY(sq1))
              Thread.sleep(10)
              MouseControl.mouseDown()
              Thread.sleep(10)
              MouseControl.moveMouse(minX + toScreenX(sq2), minY + toScreenY(sq2))
              Thread.sleep(10)
              MouseControl.mouseUp()
              MouseControl.moveMouse(beforeX, beforeY)

              if (stateAfter.winner != PlayerWinType.NotFinished) {
                println("Game Over!")
                println("Result: " + stateAfter.winner)
              } else {
                Thread.sleep(3000)
                playTurn(gameFolder, Some(stateAfter), false)
              }
            }
          } else {
            if (lastImageHadProblems) {
              println("Board with problems:")
              println(ImageLoader.pieceNamesPretty(pieceNames))

              val file = new File(s"Images/auto-screen/$name.ceo")
              val bw = new BufferedWriter(new FileWriter(file))
              bw.write(ImageLoader.pieceNamesPretty(pieceNames))
              bw.close()
              System.exit(-1)
              ???
            } else {
              println("Board with problems:")
              println(ImageLoader.pieceNamesPretty(pieceNames))
              println("Trying to take the print again...")
              Thread.sleep(1000)
              playTurn(gameFolder, gameState, true)
            }
          }
      }
    }
  }

  //  def checkEnemyPlay(
  //    afterWhiteStateOption: Option[GameState],
  //    afterBlackStateOption: Option[GameState],
  //    lastMoveCoordinates: List[BoardPos]
  //  ): Option[GameState] = {
  //    (afterWhiteStateOption, afterBlackStateOption) match {
  //      case (Some(afterWhiteState), Some(afterBlackState)) =>
  //        val allPossibleMoves =
  //          afterWhiteState.generateAllNextStates.filter {
  //            nextGameState =>
  //              1 == 1
  //              val allSquares =
  //                for (row <- 0 until 8; column <- 0 until 8) yield {
  //                  (afterBlackState.board(row, column), nextGameState.board(row, column)) match {
  //                    case (Some(piece), _) if piece.data.isUnknown =>
  //                      val r = lastMoveCoordinates.contains(piece.pos)
  //                      r
  //                    case (Some(piece1), Some(piece2)) if piece1.data.name == piece2.data.name =>
  //                      true
  //                    case (None, None) =>
  //                      true
  //                    case _ =>
  //                      false
  //                  }
  //                }
  //              allSquares.forall(identity)
  //          }
  //        if (allPossibleMoves.lengthCompare(1) == 0)
  //          allPossibleMoves.headOption
  //        else {
  //          System.err.println("Last black move has ambiguous!!!")
  //          println(allPossibleMoves.map(_.movesHistory.head).mkString("\n"))
  //          allPossibleMoves.headOption
  //        }
  //      case _ =>
  //        None
  //    }
  //  }

  def checkEnemyPlay(
    afterWhiteStateOption: Option[GameState],
    afterBlackStateOption: Option[GameState],
    lastMoveCoordinates: List[BoardPos],
    loader: ImageBoardLoader
  ): Option[GameState] = {
    (afterWhiteStateOption, afterBlackStateOption) match {
      case (Some(afterWhiteState), Some(afterBlackState)) =>
        val allNextStates = afterWhiteState.generateAllNextStates
        val allPossibleMoves =
          allNextStates.filter {
            nextGameState =>
              1 == 1
              val allSquares =
                for (row <- 0 until 8; column <- 0 until 8) yield {
                  (afterBlackState.board(row, column), nextGameState.board(row, column)) match {
                    case (Some(piece), _) if piece.data.isUnknown =>
                      true
                    case (Some(piece1), Some(piece2)) if piece1.data.name == piece2.data.name =>
                      true
                    case (None, None) =>
                      true
                    case _ =>
                      false
                  }
                }
              allSquares.forall(identity)
          }
        if (allPossibleMoves.lengthCompare(1) == 0)
          allPossibleMoves.headOption
        else {
          System.err.println("Last black move has ambiguous!!!")
          println(allPossibleMoves.map(_.movesHistory.head).mkString("\n"))

          val problematicPositions =
            BoardPos.allBoardPositions.collect {
              case boardPos if {
                val piece = boardPos.getPiece(afterBlackState.board)
                piece.exists(_.data.isUnknown)
              } => boardPos.getPiece(afterBlackState.board).get
            }

          val result: List[(BoardPos, String)] =
            problematicPositions.map { piece =>
              val pos = piece.pos
              val pieceImage = loader.getImageAt(pos.row, pos.column)
              val possiblePieceNames = allNextStates.map { state =>
                pos.getPiece(state.board).map(_.data.name).getOrElse("e")
              }.distinct
              (pos, ImageLoader.guessPieceNameWithPossibilities(pieceImage, pos, possiblePieceNames) match {
                case Nil => "?"
                case List((singleName, _)) =>
                  singleName
                case _ =>
                  ???
              })
            }

          println("possible pieces in position:")
          checkEnemyPlay(
            afterWhiteStateOption: Option[GameState],
            afterBlackStateOption: Option[GameState],
            lastMoveCoordinates: List[BoardPos],
            loader: ImageBoardLoader
          )
        }
      case _ =>
        None
    }
  }

}
