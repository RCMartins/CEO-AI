package ceo.control

import java.awt.image.BufferedImage
import java.io.{File, FileWriter, IOException}
import java.text.SimpleDateFormat
import java.util.Date

import ceo.image.ImageLoader.ImageState
import ceo.image._
import ceo.menu.Exceptions.BoardStartsWithUnknownPieces
import ceo.menu.{Exceptions, InGameMenuType, MenuControl, MenuType}
import ceo.play._

import scala.concurrent.Future

object MainControl {

  val autoScreenFolder = new File("Images/auto-screen")
  val imagesToCheckFolder = new File("Images/images-to-check")

  val minX: Int = 2220 + 1920 * -1
  val minY: Int = 312
  val sizeX: Int = 1000
  val sizeY: Int = 650

  val strategyBlitz: Strategy = buildStrategy(10000)
  val strategyDefault: Strategy = buildStrategy(25000)
  val strategyChallenge: Strategy = buildStrategy(30000)
  var strategy: Strategy = strategyDefault

  def buildStrategy(time: Int, maxMoves: Int = 50): Strategy = {
    new Strategy.ABPruningIterativeDeepening(time, maxMoves) with ValueOfState.HeuristicPieceValues
    //    new Strategy.ABPIDOrdered(time, maxMoves) with ValueOfState.HeuristicPieceValues
  }

  def start(menu: InGameMenuType, playerColor: PlayerColor): Unit = {
    if (MenuControl.DEBUG_MODE)
      strategy = MenuControl.DEBUG_STRATEGY
    ImageLoader.initialize()

    val dateFormatter = new SimpleDateFormat("yyyy-MM-dd-HH-mm-ss")
    val submittedDateConvert = new Date()
    val name = dateFormatter.format(submittedDateConvert)
    val gameFolder = new File(autoScreenFolder, name)
    val replayFile = new File(gameFolder, "replay.ceo")
    var lastSavedTurn: Double = -1
    var currentTurnImage: Option[BufferedImage] = None

    val inMultiplayer = menu match {
      case MenuType.PlayingInMultiplayer => true
      case _ => false
    }

    def playTurn(gameState: Option[GameState]): Unit = {
      if (gameState.exists(_.winner != PlayerWinType.NotFinished)) {
        println("Game Over!")
        println("Result: " + gameState.get.winner)
      } else {
        def getScreen: BufferedImage = {
          var count = 0

          def getScreenAux: BufferedImage = {
            val screen = MenuControl.captureScreen

            val loader = new ImageBoardLoader(screen)
            if (loader.isValid) {
              if (gameState.isEmpty) {
                screen
              } else {
                val currentPlayerTeam = loader.currentTeam(playerColor)
                Thread.sleep(1000)
                val screenFinal = MenuControl.captureScreen
                val loaderFinal = new ImageBoardLoader(screenFinal)
                if (loaderFinal.isValid) {
                  val finalPlayerTeam = loaderFinal.currentTeam(playerColor)
                  if (finalPlayerTeam == currentPlayerTeam) {
                    val finalTurnImage = loaderFinal.getTurnImage
                    if (finalPlayerTeam != playerColor || currentTurnImage.forall(!ImageUtils.areExactlyEqual(_, finalTurnImage)))
                      screenFinal
                    else {
                      println("Turn check failed! trying again!")
                      getScreenAux
                    }
                  } else
                    getScreenAux
                } else {
                  println("Not in game screen anymore! waiting...")
                  getScreenAux
                }
              }
            } else {
              count += 1
              if (count > 25) {
                MenuType.findMenu(screen) match {
                  case Some(MenuType.MultiplayerGameOverScreen) =>
                    throw new Exceptions.GameIsOverByAbandonOrTimeout
                  case _ => // ignore because bot never abandons games.
                }
              }

              Thread.sleep(50)
              getScreenAux
            }
          }

          getScreenAux
        }

        val currentScreen = getScreen

        def saveScreen(currentTurn: Double): Unit = {
          if (!MenuControl.DEBUG_MODE && lastSavedTurn != currentTurn) {
            lastSavedTurn = currentTurn
            val dateFormatter = new SimpleDateFormat("yyyy-MM-dd-HH-mm-ss")
            val submittedDateConvert = new Date()
            val name = dateFormatter.format(submittedDateConvert)
            ImageUtils.writeImage(currentScreen, new File(gameFolder, s"$name.png"))
          }
        }

        val piecesFromUnknownBoard: Option[ImageState] = {
          if (gameState.isEmpty)
            ImageLoader.getPiecesFromUnknownBoard(playerColor, currentScreen, showTime = true)
          else {
            if (!MenuControl.DEBUG_MODE) {
              val dateFormatter = new SimpleDateFormat("yyyy-MM-dd-HH-mm-ss-SSS")
              val submittedDateConvert = new Date()
              val name = dateFormatter.format(submittedDateConvert)
              ImageUtils.writeImage(currentScreen, new File(imagesToCheckFolder, s"$name.png"))
            }
            ImageLoader.quickCheckLastMoveCoordinates(playerColor, currentScreen)
          }
        }

        piecesFromUnknownBoard match {
          case None =>
            println("Board not found!")
          case Some(ImageState(color, _, pieceNames, _)) if color != playerColor =>
            println("Still in opponent turn, waiting...")
            saveScreen(gameState.map(_.currentTurn).getOrElse(0.0))
            gameState match {
              case None =>
                // First turn in black side so initialize the board
                val initialState = Some(ImageLoader.loadBoardFromPieceNamesNoFilter(pieceNames, playerColor == PlayerColor.White))
                playTurn(initialState)
              case _ =>
                playTurn(gameState)
            }
          case Some(ImageState(_, loader, pieceNames, lastMoveCoordinates)) =>
            currentTurnImage = Some(loader.getTurnImage)
            val stateOption = Some(ImageLoader.loadBoardFromPieceNamesNoFilter(pieceNames, playerColor == PlayerColor.White))
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

            saveScreen(gameState.map(_.currentTurn + 0.5).getOrElse(0.0))

            val checkEnemy =
              checkEnemyPlay(gameState, stateOption.map(_.board), lastMoveCoordinates, loader)

            if (gameState.isEmpty || checkEnemy.exists(_.allPieces.forall(!_.data.isUnknown))) {
              val startingState =
                if (gameState.isEmpty && playerColor == PlayerColor.Black) {
                  val state = stateOption.get
                  state.copy(currentTurn = MenuControl.StartingTurn)
                } else
                  checkEnemy.getOrElse(stateOption.get)

              if (!MenuControl.DEBUG_MODE) {
                saveStateToReplay(replayFile, startingState.getReplayInfo)
              }

              startingState.movesHistory.headOption.foreach(move => println("Enemy move: " + move))
              println(startingState)
              if (startingState.winner != PlayerWinType.NotFinished) {
                gameOver(startingState)
              } else {
                MenuControl.inPauseLoop()
                val time = System.currentTimeMillis()
                if (startingState.currentTurn < 1.0)
                  strategy.initialize(startingState)
                else
                  strategy.moveMade(startingState)
                println("Starting to calculate turn move....")
                val Some(stateAfter) = strategy.chooseMove(startingState)
                println
                val turnCalcTime = System.currentTimeMillis() - time
                println(s"Turn calc time: $turnCalcTime")
                val move = stateAfter.movesHistory.head

                println("Move done: " + move.betterHumanString)
                println("Game should look like this:")
                println(stateAfter)

                val controllerMove = move.getControllerMove
                val (pos1, pos2) = controllerMove
                val sq1 = loader.getSquare(pos1.row, pos1.column)
                val sq2 = loader.getSquare(pos2.row, pos2.column)

                def toScreenX(sq: Square): Int = sq.getCenterX.toInt

                def toScreenY(sq: Square): Int = sq.getCenterY.toInt

                val (beforeX, beforeY) = MouseKeyboardControl.getMousePosition

                def doMoveAction(): Unit = {
                  MenuControl.slowMove(minX + toScreenX(sq1), minY + toScreenY(sq1))
                  Thread.sleep(50)
                  MouseKeyboardControl.mouseDown()
                  Thread.sleep(50)
                  MenuControl.slowMove(minX + toScreenX(sq2), minY + toScreenY(sq2))
                  Thread.sleep(50)
                  MouseKeyboardControl.mouseUp()
                }

                if (inMultiplayer && turnCalcTime < 1000) {
                  Thread.sleep(1000)
                }

                if (inMultiplayer && stateAfter.winner != PlayerWinType.NotFinished) {
                  MenuControl.writeInChat(menu, "gg\n")
                  Util.Beep()
                  Thread.sleep(4000)
                }

                MenuControl.inPauseLoop()
                doMoveAction()
                MenuControl.slowMove(beforeX, beforeY)

                if (!MenuControl.DEBUG_MODE) {
                  saveStateToReplay(replayFile, stateAfter.getReplayInfo)
                }

                if (stateAfter.winner != PlayerWinType.NotFinished) {
                  gameOver(stateAfter)
                } else {
                  Thread.sleep(500)
                  strategy.moveMade(stateAfter)
                  val stateAfterOptimized = stateAfter.optimize
                  playTurn(Some(stateAfterOptimized))
                }
              }
            } else if (checkEnemy.isEmpty) {
              // game is already over ???
              // TODO this should be done by throwing an exception
            } else {
              println("Board with problems:")
              println(ImageLoader.pieceNamesPretty(pieceNames))

              val dateFormatter = new SimpleDateFormat("yyyy-MM-dd-HH-mm-ss-SSS")
              val submittedDateConvert = new Date()
              val name = dateFormatter.format(submittedDateConvert)
              val unknownScreenFolder = new File("Images", "unknown-screen")
              ImageUtils.writeImage(currentScreen, new java.io.File(unknownScreenFolder, s"$name.png"))

              Util.Beep()
              Thread.sleep(10000)
              playTurn(gameState)
            }
        }
      }
    }

    def saveStateToReplay(replayFile: File, replayText: String): Unit = Future {
      def save(): Unit = {
        var fw: FileWriter = null
        try {
          fw = new FileWriter(replayFile, true)
          fw.write(replayText)
          if (fw != null)
            fw.close()
        } catch {
          case _: IOException =>
            Thread.sleep(1000)
            if (fw != null)
              fw.close()
            save()
        }
      }

      save()
    }(scala.concurrent.ExecutionContext.Implicits.global)

    def gameOver(state: GameState): Unit = {
      println("Game Over!")
      println("Result: " + state.winner)
      menu match {
        case m @ MenuType.PlayingChallenge if state.winner == PlayerWinType.PlayerBlack =>
          MenuControl.clickWait(m.resetChallengeCoordinate, 0)
        case _ => // ignore
      }
    }

    def checkEnemyPlay(
      endOfLastTurnStateOption: Option[GameState],
      afterEnemyTurnBoardOption: Option[Board],
      lastMoveCoordinates: List[BoardPos],
      loader: ImageBoardLoader
    ): Option[GameState] = {
      (endOfLastTurnStateOption, afterEnemyTurnBoardOption) match {
        case (Some(endOfLastTurnState), Some(afterEnemyTurnBoard)) =>
          val allNextStates = endOfLastTurnState.generateAllNextStates
          val allPossibleMoves =
            allNextStates.filter {
              nextGameState =>
                val lastControllerMove = nextGameState.movesHistory.head.getControllerMove
                val set = Set(lastControllerMove._1, lastControllerMove._2)
                lastMoveCoordinates.forall(set.contains)
            }

          allPossibleMoves.map(_.movesHistory.head) match {
            case List(_) | List(_: PlayerMove.Swap, _: PlayerMove.Swap) =>
              val result @ Some(onlyPossibleState) = allPossibleMoves.headOption
              val problematicPositions =
                BoardPos.allBoardPositions.collect {
                  case boardPos if {
                    val piece = boardPos.getPiece(afterEnemyTurnBoard)
                    piece.exists(_.data.isUnknown)
                  } => boardPos -> boardPos.getPiece(onlyPossibleState.board)
                }
              val newKnowledgePiecePositions = problematicPositions.toMap -- lastMoveCoordinates
              if (newKnowledgePiecePositions.nonEmpty) {
                newKnowledgePiecePositions.foreach {
                  case (BoardPos(row, column), Some(piece)) =>
                    val pieceImage = loader.getImageAt(row, column)
                    val inWhiteSquare = BoardImageData.isBackgroundWhite(row, column)
                    ImageLoader.imagesToConfirm.enqueue((pieceImage, piece.data, inWhiteSquare))
                  case _ => // ignore
                }
              }

              result
            case _ =>
              System.err.println("Something went wrong?")
              System.err.println("Checking if the game ended because of a timeout or abandon...")
              Thread.sleep(10000)
              MenuControl.findCurrentScreenMenu() match {
                case Some(MenuType.MultiplayerGameOverScreen) =>
                  println("Game is already over!")
                  return None
                case None | Some(_: InGameMenuType) =>
                // continue with error ...
                case other =>
                  System.err.println(s"Current screen: $other")
                  System.err.println(s"Not sure how to proceed...")
                // continue ???
              }

              println(s"lastMoveCoordinates: $lastMoveCoordinates")
              println("allPossibleMoves:")
              println(allPossibleMoves.map(_.movesHistory.head).mkString("\n"))
              println("-" * 20)

              val List(state1, state2) = allPossibleMoves
              val positionsThatAreDifferent =
                BoardPos.allBoardPositions.flatMap { boardPos =>
                  (boardPos.getPiece(state1.board), boardPos.getPiece(state2.board)) match {
                    case (Some(piece1), Some(piece2)) if {
                      piece1.getReplayInfo(withPos = true, withTeam = true) == piece2.getReplayInfo(withPos = true, withTeam = true)
                    } =>
                      None
                    case (None, None) =>
                      None
                    case (pieceMaybe1, pieceMaybe2) =>
                      Some((pieceMaybe1, pieceMaybe2, boardPos))
                  }
                }

              println("positionsThatAreDifferent:")
              println(positionsThatAreDifferent)
              println("-" * 20)

              val dateFormatter = new SimpleDateFormat("yyyy-MM-dd-HH-mm-ss-SSS")
              val submittedDateConvert = new Date()
              val name = dateFormatter.format(submittedDateConvert)
              val unknownScreenFolder = new File("Images", "unknown-screen")
              ImageUtils.writeImage(loader.imageIn, new java.io.File(unknownScreenFolder, s"$name.png"))

              //          val problematicPositions =
              //            BoardPos.allBoardPositions.collect {
              //              case boardPos if {
              //                val piece = boardPos.getPiece(afterBlackState.board)
              //                piece.exists(_.data.isUnknown)
              //              } => boardPos.getPiece(afterBlackState.board).get
              //            }
              //
              //          val result: List[(BoardPos, String)] =
              //            problematicPositions.map { piece =>
              //              val pos = piece.pos
              //              val pieceImage = loader.getImageAt(pos.row, pos.column)
              //              val possiblePieceNames = allNextStates.map { state =>
              //                pos.getPiece(state.board).map(_.data.name).getOrElse("e")
              //              }.distinct
              //              (pos, ImageLoader.guessPieceNameWithPossibilities(pieceImage, pos, possiblePieceNames) match {
              //                case Nil => "?"
              //                case List((singleName, _)) =>
              //                  singleName
              //                case _ =>
              //                  ???
              //              })
              //            }

              Util.Beep5()
              var reTry = false
              if (reTry)
                None
              else {
                checkEnemyPlay(
                  endOfLastTurnStateOption,
                  afterEnemyTurnBoardOption,
                  lastMoveCoordinates,
                  loader
                )
              }
          }
        case _ =>
          None
      }
    }

    playTurn(None)
  }
}
