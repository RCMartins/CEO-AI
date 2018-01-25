package ceo.menu

import java.awt.Rectangle

import ceo.control.MainControl.{minX, minY, sizeX, sizeY}
import ceo.control.{MainControl, MouseControl}
import ceo.menu.Exceptions.{AllPiecesAreKnown, BoardStartsWithUnknownPieces}
import ceo.menu.OathObjective._
import ceo.play.Util
import ceo.ui.MainPlayUI

import scala.util.{Failure, Success, Try}

object MenuControl {

  val oathObjective: OathObjective = OathObjective.EditArmyTesting

  def main(args: Array[String]): Unit = {
    MainPlayUI.start()
    start()
  }

  private val DEFAULT_TIMEOUT = 1000
  private val MAX_TIMEOUT = 60000

  private var timeOutFromNotFindingAnyMenu = DEFAULT_TIMEOUT
  private var settingsReady = false
  private var challengeAlreadyWon = false

  def start(): Unit = {
    val screen = MouseControl.robot.createScreenCapture(new Rectangle(minX, minY, sizeX, sizeY))

    def controlGame(currentMenu: MenuType): Unit = currentMenu match {
      case menu @ MenuType.MainMenuCanOpenBox =>
        println("In 'MainMenuCanOpenBox'")
        clickWaitRestart(menu.boxOpenCoordinate, 5000)
      case menu @ MenuType.MainMenuNoOpenBox =>
        println("In 'MainMenuNoOpenBox'")
        oathObjective match {
          case EditArmyTesting =>
            clickWaitRestart(menu.editArmyCoordinate, 2500)
          case CompleteDailyChallenge if !challengeAlreadyWon =>
            clickWaitRestart(menu.beginCombatCoordinate, 2500)
          case CompleteDailyChallenge =>
            println("All done for today!\nShutting down...")
          case PlayCasualGames =>
            clickWaitRestart(menu.beginCombatCoordinate, 2500)
        }
      case menu @ MenuType.OpenBoxMenu =>
        println("In 'OpenBoxMenu'")
        Wait(4000)
        clickWait(menu.chooseBoxCoordinate, 4000)
        clickWait(menu.chooseBoxCoordinate, 4000)
        clickWaitRestart(menu.confirmCoordinate, 2000)
      case menu @ MenuType.BeginCombatMenu =>
        println("In 'BeginCombatMenu'")
        oathObjective match {
          case EditArmyTesting =>
            clickWaitRestart(menu.exitCoordinate, 2000)
          case CompleteDailyChallenge =>
            clickWaitRestart(menu.challengeButtonCoordinate, 5000)
          case PlayCasualGames =>
            clickWaitRestart(menu.casualButtonCoordinate, 5000)
        }
      case menu @ MenuType.EditArmyMenu =>
        println("In 'EditArmyMenu'")
        if (oathObjective == EditArmyTesting) {
          clickWait(menu.selectArmyCoordinate(10), 200)
          clickWait(menu.randomArmyCoordinate, 1000)
          clickWait(menu.saveArmyCoordinate, 2000)
          clickWaitRestart(menu.testArmyCoordinate, 3000)
        } else {
          clickWaitRestart(menu.returnToTitleCoordinate, 2000)
        }
      case menu @ MenuType.PlayingTesting =>
        println("In 'PlayingTesting'")
        if (oathObjective == EditArmyTesting) {
          checkOptions(menu)
        } else if (oathObjective == CompleteDailyChallenge) {
          clickWaitRestart(menu.exitPlayingCoordinate, 4000)
        }
      case menu @ MenuType.TodaysChallengeWindow =>
        println("In 'TodaysChallengeWindow'")
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
        println("In 'PlayingChallenge'")
        if (oathObjective == EditArmyTesting) {
          clickWaitRestart(menu.exitPlayingCoordinate, 4000)
        } else if (oathObjective == CompleteDailyChallenge) {
          checkOptions(menu)
        }
      case menu @ MenuType.ChallengeVictoryWindow =>
        println("In 'ChallengeVictoryWindow'")
        clickWaitRestart(menu.okButtonCoordinate, 4000)
      case MenuType.FindingPracticePartner =>
        println("In 'FindingPracticePartner'")
        Wait(2000)
        start()
      case menu @ MenuType.PlayingInMultiplayer =>
        println("In 'PlayingInMultiplayer'")
        checkOptions(menu)
      case menu @ MenuType.MultiplayerGameOverScreen =>
        println("In 'MultiplayerGameOverScreen'")
        clickWaitRestart(menu.returnToTitleCoordinate, 4000)
      case menu @ MenuType.SelectTestMethod =>
        println("In 'SelectTestMethod'")
        clickWaitRestart(menu.startCoordinate, 5000)
    }

    def checkOptions(menu: InGameMenuType): Unit = {
      if (MenuType.menuMatch(screen, menu.notInSettingsTab)) {
        println("In 'notInSettingsTab'")
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
          start()
        }
      } else if (MenuType.menuMatch(screen, menu.inSettingsTabColorsDisabled)) {
        println("In 'SettingsTabColorsDisabled'")
        clickWait(menu.deSelectBoardCoordinate, 400)
        settingsReady = true
        startPlaying(menu)
      }
    }

    def startPlaying(menu: InGameMenuType): Unit = {
      Try(MainControl.start()) match {
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
            case CompleteDailyChallenge | PlayCasualGames =>
              Util.Beep()
              println("Board started with unknown pieces, let's wait 10s for the images to be updated...")
              Thread.sleep(10000)
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
      MouseControl.moveMouse(minX + x, minY + y)
      Thread.sleep(10)
      MouseControl.mouseDown()
      Thread.sleep(10)
      MouseControl.mouseUp()
      MouseControl.moveMouse(beforeX, beforeY)

      Wait(waitTime)
    }

    def clickWaitRestart(mouseCoordinate: (Int, Int), waitTime: Int): Unit = {
      clickWait(mouseCoordinate, waitTime)
      start()
    }

    MenuType.findMenu(screen) match {
      case None =>
        println("Error, can't figure out which menu it's in...")
        Thread.sleep(timeOutFromNotFindingAnyMenu)
        timeOutFromNotFindingAnyMenu = Math.min(MAX_TIMEOUT, timeOutFromNotFindingAnyMenu + 100)
        start()
      case Some(currentMenu) =>
        timeOutFromNotFindingAnyMenu = DEFAULT_TIMEOUT
        controlGame(currentMenu)
    }
  }

}
