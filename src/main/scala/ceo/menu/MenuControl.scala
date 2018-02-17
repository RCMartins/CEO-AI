package ceo.menu

import java.awt.image.BufferedImage
import java.io.File
import java.text.SimpleDateFormat
import java.util.Date

import ceo.control.MainControl.{minX, minY}
import ceo.control.{MainControl, MouseKeyboardControl}
import ceo.image.{ImageLoader, ImageUtils}
import ceo.menu.Exceptions.{AllPiecesAreKnown, BoardStartsWithUnknownPieces, GameIsOverByAbandonOrTimeout}
import ceo.menu.OathObjective._
import ceo.play
import ceo.play.ValueOfState.ImprovedHeuristic
import ceo.play.{PlayerColor, Strategy, Util}
import ceo.ui.MainPlayUI

import scala.util.{Failure, Success, Try}

object MenuControl {

  var USE_BEST_GUESS_IMAGES = false
  val DEBUG_MODE = false
  val SHOW_PRINTS = true
  val StartingTurn: Double = 0.5
  val DEBUG_STRATEGY_TIME: Int = 100000000
  val DEBUG_STRATEGY_TURNS: Int = 1
  val DEBUG_STRATEGY: Strategy = new play.Strategy.ABPruningIterativeDeepening(DEBUG_STRATEGY_TIME, DEBUG_STRATEGY_TURNS) with ImprovedHeuristic
  var oathObjectiveList: List[OathObjective] = List(
    OathObjective.PlayRankedGames,
    OathObjective.PlayCurrentGame,
    OathObjective.CompleteDailyChallenge,
  )

  def oathObjective: OathObjective = oathObjectiveList.head

  def main(args: Array[String]): Unit = {
    MainPlayUI.start()
    if (oathObjective.involvesPlayingGames)
      ImageLoader.initialize()
    start()
  }

  private val DEFAULT_TIMEOUT = 1000
  private val MAX_TIMEOUT = 60000

  private var timeOutFromNotFindingAnyMenu = DEFAULT_TIMEOUT
  private var settingsReady = false
  private var challengeAlreadyWon = false

  var inPause: Boolean = false
  var pauseAfterGameFinishes: Boolean = false

  var debugImagesList: List[BufferedImage] = List(
    javax.imageio.ImageIO.read(new java.io.File("Images/auto-screen/2018-02-10-22-09-44/2018-02-10-22-14-08.png")),
  ).flatMap(image => List(image, image))

  def captureScreen: BufferedImage = {
    if (DEBUG_MODE) {
      import javax.imageio.ImageIO.read
      //      read(new java.io.File("Images/auto-screen/2018-02-05-22-26-48/2018-02-05-22-27-20.png"))

      val head :: tail = debugImagesList
      debugImagesList = tail
      head
    } else {
      MouseKeyboardControl.robot.createScreenCapture(new java.awt.Rectangle(minX, minY, MainControl.sizeX, MainControl.sizeY))
    }
  }

  def start(): Unit = {
    inPauseLoop()

    val screen = captureScreen

    def controlGame(currentMenu: MenuType): Unit = {
      println(s"In '$currentMenu'")
      currentMenu match {
        case menu @ MenuType.WelcomeContinueMenu =>
          settingsReady = false
          clickWait(menu.continueCoordinate, 9000)
          clickWaitRestart(menu.skipCoordinate, 6000)
        case menu @ MenuType.MainMenuCanOpenBox =>
          clickWaitRestart(menu.boxOpenCoordinate, 5000)
        case menu @ MenuType.MainMenuNoOpenBox =>
          oathObjective match {
            case EditArmyTesting =>
              clickWaitRestart(menu.editArmyCoordinate, 2500)
            case CompleteDailyChallenge if !challengeAlreadyWon =>
              clickWaitRestart(menu.beginCombatCoordinate, 2500)
            case CompleteDailyChallenge =>
              println("Today's challenge done! Switching mode...")
              nextOathObjective()
              start()
            case PlayCasualGames | PlayRankedGames =>
              clickWaitRestart(menu.beginCombatCoordinate, 2500)
            case PlayCurrentGame =>
              Wait(5000)
              start()
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
            case PlayCurrentGame =>
              Wait(5000)
              start()
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
          if (oathObjective == EditArmyTesting || oathObjective == PlayCurrentGame) {
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
          } else if (oathObjective != PlayCurrentGame) {
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
        case menu @ MenuType.MainMenuHasLandmarks =>
          clickWaitRestart(menu.viewLandmarksCoordinate, 2000)
        case menu @ MenuType.LandmarksMenu =>
          for (index <- 0 to 3)
            clickWait(menu.claimButtonCoordinate(index), 0)
          Wait(1000)
          clickWait(menu.okCoordinate, 1000)
          clickWait(menu.okCoordinate, 1000)
          clickWaitRestart(menu.exitCoordinate, 2000)
        case menu: OkMenu =>
          val (coordinate, time) = menu.okCoordinate
          clickWaitRestart(coordinate, time)
      }
    }

    def checkOptions(menu: InGameMenuType): Unit = {
      if (DEBUG_MODE)
        startPlaying(menu)
      else if (MenuType.menuMatch(screen, menu.notInSettingsTab)) {
        println("In 'NotInSettingsTab'")
        if (settingsReady)
          startPlaying(menu)
        else
          clickWaitRestart(menu.settingsTabCoordinate, 4000)
      } else if (MenuType.menuMatch(screen, menu.inSettingsTabColorsEnabled)) {
        println("In 'SettingsTabColorsEnabled'")
        if (settingsReady) {
          if (!DEBUG_MODE)
            clickWait(menu.unitsTakenTabCoordinate, 250)
          startPlaying(menu)
        } else {
          for (index <- 0 until 5)
            clickWait(menu.settingCoordinate(index), 0)
          clickWait(menu.deSelectBoardCoordinate, 400)
          start()
        }
      } else if (MenuType.menuMatch(screen, menu.inSettingsTabColorsDisabled)) {
        println("In 'SettingsTabColorsDisabled'")
        settingsReady = true
        if (!DEBUG_MODE)
          clickWait(menu.unitsTakenTabCoordinate, 250) // TODO Create a inUnitsTakenTab
        startPlaying(menu)
      }
    }

    def startPlaying(menu: InGameMenuType): Unit = {
      val playerColor =
        if (MenuType.menuMatch(screen, menu.isWhitePlayerAtBottom))
          PlayerColor.White
        else
          PlayerColor.Black

      menu match {
        case MenuType.PlayingChallenge =>
          MainControl.strategy = MainControl.strategyChallenge
        case m @ MenuType.PlayingInMultiplayer if MenuType.menuMatch(screen, m.inBlitzGame) =>
          MainControl.strategy = MainControl.strategyBlitz
        case _ =>
          MainControl.strategy = MainControl.strategyDefault
      }

      Try(MainControl.start(menu, playerColor)) match {
        case Success(_) | Failure(_: GameIsOverByAbandonOrTimeout) =>
          println("Game Over!")
          val time = System.currentTimeMillis()
          ImageLoader.possibleNewImagesQueue.foreach(_ ())
          ImageLoader.possibleNewImagesQueue.clear()
          Wait(15000 - (System.currentTimeMillis() - time).toInt)
          if (pauseAfterGameFinishes) {
            var waitingCounter = 0
            while (pauseAfterGameFinishes) {
              if (waitingCounter % 10 == 0)
                println("Game finished... Waiting for resume...")
              waitingCounter += 1
              Thread.sleep(1000)
            }
          }
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
            case CompleteDailyChallenge | PlayCasualGames | PlayRankedGames | PlayCurrentGame =>
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

    MenuType.findMenu(screen) match {
      case None =>
        println("Error, can't figure out which menu it's in...")

        val dateFormatter = new SimpleDateFormat("yyyy-MM-dd-HH-mm-ss-SSS")
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

  def findCurrentScreenMenu(): Option[MenuType] =
    MenuType.findMenu(captureScreen)

  def Wait(waitTime: Int): Unit = {
    Thread.sleep(Math.max(0, waitTime))
  }

  def clickWait(mouseCoordinate: (Int, Int), waitTime: Int): Unit = {
    val (x, y) = mouseCoordinate
    val (beforeX, beforeY) = MouseKeyboardControl.getMousePosition
    slowMove(minX + x, minY + y)
    Thread.sleep(50)
    MouseKeyboardControl.mouseDown()
    Thread.sleep(50)
    MouseKeyboardControl.mouseUp()
    slowMove(beforeX, beforeY)

    Wait(waitTime)
  }

  def clickWaitRestart(mouseCoordinate: (Int, Int), waitTime: Int): Unit = {
    clickWait(mouseCoordinate, waitTime)
    start()
  }

  def slowMove(x: Int, y: Int): Unit = {
    val (ix, iy) = MouseKeyboardControl.getMousePosition
    if (ix != x || iy != y) {
      val distX = Math.min(x - ix, Math.max(20, (x - ix) / 8))
      val distY = Math.min(y - iy, Math.max(20, (y - iy) / 8))
      MouseKeyboardControl.moveMouse(ix + distX, iy + distY)
      Thread.sleep(15)
      slowMove(x, y)
    }
  }

  def inPauseLoop(): Unit = {
    var waitingCounter = 0
    while (inPause) {
      if (waitingCounter % 10 == 0)
        println("waiting for resume...")
      waitingCounter += 1
      Thread.sleep(1000 + waitingCounter * 10)
    }
  }

  def nextOathObjective(): Unit = {
    oathObjectiveList match {
      case _ :: _ :: _ =>
        oathObjectiveList = oathObjectiveList.tail
      case _ => // ignore
    }
  }

  def writeInChat(menu: InGameMenuType, string: String): Unit = {
    menu match {
      case inGameMenu: InGameMenuType =>
        clickWait(inGameMenu.chatCoordinate, 500)
        MouseKeyboardControl.typeText(string)
      case _ => // ignore?
    }
  } //

}
