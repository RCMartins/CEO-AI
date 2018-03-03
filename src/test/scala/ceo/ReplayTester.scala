package ceo

import java.io.File

import ceo.image.ImageLoader
import ceo.play.PlayerTeam.{BlackTop, WhiteBottom}
import ceo.play._
import org.scalatest
import org.scalatest.{AppendedClues, MustMatchers, OptionValues, WordSpec}

import scala.io.Source

class ReplayTester
  extends WordSpec
    with MustMatchers
    with OptionValues
    with AppendedClues {

  DataLoader.loadPieceFiles()

  private val replayFolder = new File("Images/auto-screen")

  case class TurnData(
    currentTurn: Double,
    currentPlayerTeam: PlayerTeam,
    board: Board,
    playerWhite: Player,
    playerBlack: Player,
    boardEffects: List[BoardEffect]
  )

  def loadReplayFolder(file: File): Unit = {
    if (file.isDirectory)
      file.listFiles().foreach(loadReplayFolder)
    else if (file.getName == "replay.ceo") {
      val shorterName = file.getAbsolutePath.stripPrefix(replayFolder.getAbsolutePath)
      s"replay '$shorterName'" should {
        "load and play correctly" in {
          loadReplayFile(file)
        }
      }
    }
  }

  def parsePlayerTeam(currentPlayerTeamStr: String): PlayerTeam = {
    PlayerTeam.all.find(_.toString == currentPlayerTeamStr).value withClue s"PlayerTeam '$currentPlayerTeamStr' is not valid"
  }

  def parseBoardReplayInfo(boardStringList: List[String], whitePlayerInBottom: Boolean): GameState = {
    val pieceNames = boardStringList.map(_.split(" ").toList.map(_.takeWhile(_ != '[').stripSuffix("Top").stripSuffix("Bottom")))
    ImageLoader.loadBoardFromPieceNamesNoFilter(
      pieceNames = pieceNames,
      whitePlayerInBottom = whitePlayerInBottom
    )
  }

  def loadReplayFile(file: File): Unit = {
    val lines = Source.fromFile(file).getLines.toList

    val (firstTurnMaybe, others) = lines.span(_ != GameState.replayTurnSeparator)
    val firstTurn =
      if (firstTurnMaybe.lengthCompare(26) == 0)
        firstTurnMaybe
      else
        firstTurnMaybe.drop(2) // ignore first opponent white move

    val firstLine :: separator :: boardAndStuff = firstTurn
    val List(_, turnStr, currentPlayerTeamStr) = firstLine.split(" ").toList
    separator mustEqual GameState.replaySmallSeparator
    val playerTeam = parsePlayerTeam(currentPlayerTeamStr)
    val initialState =
      parseBoardReplayInfo(boardAndStuff.take(8), playerTeam == WhiteBottom || playerTeam == BlackTop)
        .copy(currentTurn = turnStr.toDouble)

    testFullReplay(others.tail, initialState)
  }

  def testFullReplay(lines: List[String], currentState: GameState): Unit = {
    if (lines.nonEmpty) {
      val (moveAndNextTurn, others) = lines.span(_ != GameState.replayTurnSeparator)
      moveAndNextTurn.size mustEqual 28 withClue moveAndNextTurn
      val move :: separator :: _ = moveAndNextTurn
      separator mustEqual GameState.replaySmallSeparator
      if (currentState.winner != PlayerWinType.NotFinished) {
        (fail(): scalatest.Assertion) withClue s"\nGame should already be over!\n${moveAndNextTurn.mkString("\n")}\n"
      } else {
        val playerMoves = currentState.getCurrentPlayerMoves
        playerMoves.find(_.toString == move) match {
          case None =>
            (fail(): scalatest.Assertion) withClue s"\nImpossible move was done!\n${moveAndNextTurn.mkString("\n")}\nMove in replay: $move\n"
          case Some(playerMove) =>
            val nextState = currentState.playPlayerMove(playerMove, turnUpdate = true)
            val nextStateReplayInfo = nextState.getReplayInfo.split("\n").toList.init
            nextStateReplayInfo mustEqual moveAndNextTurn withClue "\n" + {
              moveAndNextTurn.zip(nextStateReplayInfo).map {
                case (a, b) if a == b => s"    |$a"
                case (a, b) if a != b => s"--- |$a\n+++ |$b"
              }.mkString("\n")
            }
            testFullReplay(others.tail, nextState)
        }
      }
    }
  }

  loadReplayFolder(replayFolder)
}
