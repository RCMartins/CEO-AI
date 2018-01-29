package ceo.menu

import java.awt.image.BufferedImage
import java.io.File
import java.text.SimpleDateFormat
import java.util.Date

import ceo.control.MainControl.{minX, minY}
import ceo.control.{MainControl, MouseControl}
import ceo.image.{ImageLoader, ImageUtils}
import ceo.menu.Exceptions.{AllPiecesAreKnown, BoardStartsWithUnknownPieces}
import ceo.menu.OathObjective._
import ceo.play.{PlayerColor, Util}
import ceo.ui.MainPlayUI

import scala.util.{Failure, Success, Try}

object MenuControl {

  val USE_BEST_GUESS_IMAGES = false
  val DEBUG_MODE = false
  val StartingTurn: Double = 0.0
  val oathObjective: OathObjective = OathObjective.PlayRankedGames

  def main(args: Array[String]): Unit = {
    if (oathObjective.involvesPlayingGamesTimed)
      ImageLoader.initialize()
    MainPlayUI.start()
    start()
  }

  private val DEFAULT_TIMEOUT = 1000
  private val MAX_TIMEOUT = 60000

  private var timeOutFromNotFindingAnyMenu = DEFAULT_TIMEOUT
  private var settingsReady = false
  private var challengeAlreadyWon = false

  def captureScreen: BufferedImage = {
    if (DEBUG_MODE)
      javax.imageio.ImageIO.read(new java.io.File("Images/unknown-screen/2018-01-28-22-55-35-0500.png"))
    else
      MouseControl.robot.createScreenCapture(new java.awt.Rectangle(minX, minY, MainControl.sizeX, MainControl.sizeY))
  }

  def start(): Unit = {
    val screen = captureScreen

    def controlGame(currentMenu: MenuType): Unit = {
      println(s"In '$currentMenu'")
      currentMenu match {
        case menu @ MenuType.MainMenuCanOpenBox =>
          clickWaitRestart(menu.boxOpenCoordinate, 5000)
        case menu @ MenuType.MainMenuNoOpenBox =>
          oathObjective match {
            case EditArmyTesting =>
              clickWaitRestart(menu.editArmyCoordinate, 2500)
            case CompleteDailyChallenge if !challengeAlreadyWon =>
              clickWaitRestart(menu.beginCombatCoordinate, 2500)
            case CompleteDailyChallenge =>
              println("All done for today!\nShutting down...")
            case PlayCasualGames | PlayRankedGames =>
              clickWaitRestart(menu.beginCombatCoordinate, 2500)
          }
        case menu @ MenuType.OpenBoxMenu =>
          Wait(3000)
          clickWait(menu.chooseBoxCoordinate, 3000)
          clickWait(menu.chooseBoxCoordinate, 2000)
          clickWaitRestart(menu.confirmCoordinate, 2000)
        case menu @ MenuType.BeginCombatMenu =>
          oathObjective match {
            case EditArmyTesting =>
              clickWaitRestart(menu.exitCoordinate, 2000)
            case CompleteDailyChallenge =>
              clickWaitRestart(menu.challengeButtonCoordinate, 5000)
            case PlayCasualGames =>
              clickWaitRestart(menu.casualButtonCoordinate, 5000)
            case PlayRankedGames =>
              clickWaitRestart(menu.rankedButtonCoordinate, 5000)
          }
        case menu @ MenuType.EditArmyMenu =>
          if (oathObjective == EditArmyTesting) {
            clickWait(menu.selectArmyCoordinate(10), 200)
            clickWait(menu.randomArmyCoordinate, 1000)
            clickWait(menu.saveArmyCoordinate, 2000)
            clickWaitRestart(menu.testArmyCoordinate, 3000)
          } else {
            clickWaitRestart(menu.returnToTitleCoordinate, 2000)
          }
        case menu @ MenuType.PlayingTesting =>
          if (oathObjective == EditArmyTesting) {
            checkOptions(menu)
          } else {
            clickWaitRestart(menu.exitPlayingCoordinate, 4000)
          }
        case menu @ MenuType.TodaysChallengeWindow =>
          if (oathObjective == CompleteDailyChallenge) {
            clickWait(menu.claimPrizeCoordinate, 1500)
            clickWait(menu.claimPrizeOkButtonCoordinate, 500)
            if (MenuType.menuMatch(screen, menu.challengeWon))
              challengeAlreadyWon = true
            if (challengeAlreadyWon)
              clickWaitRestart(menu.exitCoordinate, 2000)
            else
              clickWaitRestart(menu.playChallengeCoordinate, 10000)
          } else {
            clickWaitRestart(menu.exitCoordinate, 2000)
          }
        case menu @ MenuType.PlayingChallenge =>
          //        if (oathObjective == CompleteDailyChallenge) {
          checkOptions(menu)
        //        } else  {{{ if in first turn -> exit else play ??? }}}
        //          clickWaitRestart(menu.exitPlayingCoordinate, 4000)
        //        }
        case menu @ MenuType.ChallengeVictoryWindow =>
          clickWaitRestart(menu.okButtonCoordinate, 4000)
        case MenuType.FindingPracticePartner | MenuType.SearchingForOpponent =>
          Wait(2000)
          start()
        case menu @ MenuType.PlayingInMultiplayer =>
          checkOptions(menu)
        case menu @ MenuType.MultiplayerGameOverScreen =>
          clickWaitRestart(menu.returnToTitleCoordinate, 4000)
        case menu @ MenuType.SelectTestMethod =>
          clickWaitRestart(menu.startCoordinate, 5000)
        case menu: OkMenu =>
          val (coordinate, time) = menu.okCoordinate
          clickWaitRestart(coordinate, time)
      }
    }

    def checkOptions(menu: InGameMenuType): Unit = {
      if (MenuType.menuMatch(screen, menu.notInSettingsTab)) {
        println("In 'NotInSettingsTab'")
        if (settingsReady)
          startPlaying(menu)
        else
          clickWaitRestart(menu.settingsTabCoordinate, 4000)
      } else if (MenuType.menuMatch(screen, menu.inSettingsTabColorsEnabled)) {
        println("In 'SettingsTabColorsEnabled'")
        if (settingsReady)
          startPlaying(menu)
        else {
          for (index <- 0 until 5)
            clickWait(menu.settingCoordinate(index), 200)
          clickWait(menu.deSelectBoardCoordinate, 400)
          start()
        }
      } else if (MenuType.menuMatch(screen, menu.inSettingsTabColorsDisabled)) {
        println("In 'SettingsTabColorsDisabled'")
        settingsReady = true
        startPlaying(menu)
      }
    }

    def startPlaying(menu: InGameMenuType): Unit = {
      val playerColor =
        if (MenuType.menuMatch(screen, menu.isWhitePlayerAtBottom))
          PlayerColor.White
        else
          PlayerColor.Black

      clickWait(menu.unitsTakenTabCoordinate, 250)
      Try(MainControl.start(playerColor)) match {
        case Success(_) =>
          println("Game existed without any error! Hurray!")
          Wait(15000)
          start()
        case Failure(_: BoardStartsWithUnknownPieces) =>
          oathObjective match {
            case EditArmyTesting =>
              println("Board started with unknown pieces, let's try another!")
              menu match {
                case playingTesting @ MenuType.PlayingTesting =>
                  clickWaitRestart(playingTesting.exitPlayingCoordinate, 4000)
                case _ =>
                  ???
              }
            case CompleteDailyChallenge | PlayCasualGames | PlayRankedGames =>
              Util.Beep()
              println("Board started with unknown pieces, let's wait 5s for the images to be updated...")
              Thread.sleep(5000)
              start()
          }
        case Failure(_: AllPiecesAreKnown) =>
          println("Board started with all the pieces, who cares!")
          menu match {
            case playingTesting @ MenuType.PlayingTesting =>
              clickWaitRestart(playingTesting.exitPlayingCoordinate, 4000)
            case _ =>
              ???
          }
        case Failure(e) =>
          throw e
      }
    }

    def Wait(waitTime: Int): Unit = {
      Thread.sleep(waitTime)
    }

    def clickWait(mouseCoordinate: (Int, Int), waitTime: Int): Unit = {
      val (x, y) = mouseCoordinate
      val (beforeX, beforeY) = MouseControl.getMousePosition
      slowMove(minX + x, minY + y)
      Thread.sleep(50)
      MouseControl.mouseDown()
      Thread.sleep(50)
      MouseControl.mouseUp()
      slowMove(beforeX, beforeY)

      Wait(waitTime)
    }

    def clickWaitRestart(mouseCoordinate: (Int, Int), waitTime: Int): Unit = {
      clickWait(mouseCoordinate, waitTime)
      start()
    }

    MenuType.findMenu(screen) match {
      case None =>
        println("Error, can't figure out which menu it's in...")

        val dateFormatter = new SimpleDateFormat("yyyy-MM-dd-HH-mm-ss-SSSS")
        val submittedDateConvert = new Date()
        val name = dateFormatter.format(submittedDateConvert)
        val unknownScreenFolder = new File("Images", "unknown-screen")
        unknownScreenFolder.mkdirs()
        ImageUtils.writeImage(screen, new java.io.File(unknownScreenFolder, s"$name.png"))

        Thread.sleep(timeOutFromNotFindingAnyMenu)
        timeOutFromNotFindingAnyMenu = Math.min(MAX_TIMEOUT, timeOutFromNotFindingAnyMenu + 100)
        start()
      case Some(currentMenu) =>
        timeOutFromNotFindingAnyMenu = DEFAULT_TIMEOUT
        controlGame(currentMenu)
    }
  }

  def slowMove(x: Int, y: Int): Unit = {
    val (ix, iy) = MouseControl.getMousePosition
    if (ix != x || iy != y) {
      val distX = Math.min(x - ix, Math.max(20, (x - ix) / 8))
      val distY = Math.min(y - iy, Math.max(20, (y - iy) / 8))
      MouseControl.moveMouse(ix + distX, iy + distY)
      Thread.sleep(15)
      slowMove(x, y)
    }
  }

}
