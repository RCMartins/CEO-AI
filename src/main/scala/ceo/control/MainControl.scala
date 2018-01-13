package ceo.control

import java.awt.Rectangle
import java.io.{BufferedWriter, File, FileWriter}
import java.text.SimpleDateFormat
import java.util.Date

import ceo.image.ImageLoader.ImageState
import ceo.image.{ImageLoader, ImageUtils, Square}
import ceo.play._

object MainControl {

  private val minX = 2222
  private val minY = 314
  private val sizeX = 996
  private val sizeY = 646

    val strategy = Strategy.AlphaBetaPruning(7)

  def main(args: Array[String]): Unit = {
    ImageLoader.initialize()
    playTurn(None)
  }

  def playTurn(gameState: Option[GameState]): Unit = {
    val screen = MouseControl.robot.createScreenCapture(new Rectangle(minX, minY, sizeX, sizeY))

    ImageLoader.getPiecesFromUnknownBoard(screen) match {
      case None =>
        println("Board not found!")
      case Some(ImageState(PlayerTeam.Black, _, _, _)) =>
        println("Still in Black turn, waiting...")
        // TODO use this information to detect errors in state update
        Thread.sleep(2000)
        playTurn(gameState, false)
      case Some(ImageState(PlayerTeam.White, loader, pieceNames, lastMoveCoordinates)) =>
        val stateOption = ImageLoader.loadBoardFromPieceNamesNoFilter(pieceNames)

        val dateFormatter = new SimpleDateFormat("yyyy-MM-dd-HH-mm-ss")
        val submittedDateConvert = new Date()
        val name = dateFormatter.format(submittedDateConvert)
        ImageUtils.writeImage(screen, s"PRINTS/auto-screen/$name.png")

        val checkEnemy =
          checkEnemyPlay(gameState, stateOption, lastMoveCoordinates)

        if (gameState.isEmpty || checkEnemy.exists(_.allPieces.forall(!_.data.isUnknown))) {
          val startingState = checkEnemy.getOrElse(stateOption.get)
          println(startingState.getBoardPieceNames)
          if (startingState.winner != PlayerWinType.NotFinished) {
            println("Game Over!")
            println("Result: " + startingState.winner)
          } else {

            val time = System.currentTimeMillis()
            val stateAfter = strategy.chooseMove(startingState)
            println(s"Turn calc time: ${System.currentTimeMillis() - time}")
            val move = stateAfter.get.movesHistory.head

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
            Thread.sleep(2000)

            if (stateAfter.exists(_.winner != PlayerWinType.NotFinished)) {
              println("Game Over!")
              println("Result: " + stateAfter.get.winner)
            } else {
              playTurn(stateAfter, false)
            }
          }
        } else {
          if (lastImageHadProblems) {
            println("Board with problems:")
            println(ImageLoader.pieceNamesPretty(pieceNames))

            val file = new File(s"PRINTS/auto-screen/$name.ceo")
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
            playTurn(gameState, true)
          }
        }
    }
  }

  def checkEnemyPlay(
    afterWhiteStateOption: Option[GameState],
    afterBlackStateOption: Option[GameState],
    lastMoveCoordinates: List[BoardPos]
  ): Option[GameState] = {
    (afterWhiteStateOption, afterBlackStateOption) match {
      case (Some(afterWhiteState), Some(afterBlackState)) =>
        val allPossibleMoves =
          afterWhiteState.generateAllNextStates.filter {
            nextGameState =>
              1 == 1
              val allSquares =
                for (row <- 0 until 8; column <- 0 until 8) yield {
                  (afterBlackState.board(row, column), nextGameState.board(row, column)) match {
                    case (Some(piece), _) if piece.data.isUnknown =>
                      val r = lastMoveCoordinates.contains(piece.pos)
                      r
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
          allPossibleMoves.headOption
        }
      case _ =>
        None
    }
  }

}
