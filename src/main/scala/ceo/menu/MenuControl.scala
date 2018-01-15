package ceo.menu

import java.awt.Rectangle

import ceo.control.MainControl.{minX, minY, sizeX, sizeY}
import ceo.control.{MainControl, MouseControl}
import ceo.menu.Exceptions.BoardStartsWithUnknownPieces
import ceo.ui.MainPlayUI

import scala.util.{Failure, Success, Try}

object MenuControl {

  def main(args: Array[String]): Unit = {
    MainPlayUI.start()
    start()
  }

  def start(): Unit = {
    val screen = MouseControl.robot.createScreenCapture(new Rectangle(minX, minY, sizeX, sizeY))

    MenuType.findMenu(screen) match {
      case None =>
        println("Error, can't figure out which menu it's in...")
        Thread.sleep(20000)
        start()
      case Some(start) =>
        controlGame(start)
    }
  }

  val modeEditArmyTesting = true

  def controlGame(currentMenu: MenuType): Unit = currentMenu match {
    case menu @ MenuType.MainMenuCanOpenBox =>
      println("In MainMenuCanOpenBox!")
      clickWaitRestart(menu.boxOpenCoordinate, 5000)
    case menu @ MenuType.MainMenuNoOpenBox =>
      println("In MainMenuNoOpenBox!")
      if (modeEditArmyTesting) {
        clickWaitRestart(menu.editArmyCoordinate, 2500)
      }
    case menu @ MenuType.OpenBoxMenu =>
      println("In OpenBoxMenu!")
      Wait(4000)
      clickWait(menu.chooseBoxCoordinate, 4000)
      clickWait(menu.chooseBoxCoordinate, 4000)
      clickWaitRestart(menu.confirmCoordinate, 2000)
    case MenuType.BeginCombatMenu =>
      println("In BeginCombatMenu!")
    case menu @ MenuType.EditArmyMenu =>
      println("In EditArmyMenu!")
      if (modeEditArmyTesting) {
        clickWait(menu.selectArmyCoordinate(10), 200)
        clickWait(menu.randomArmyCoordinate, 1000)
        clickWait(menu.saveArmyCoordinate, 2000)
        clickWait(menu.testArmyCoordinate, 3000)
        clickWaitRestart(menu.testArmyStartCoordinate, 5000)
      } else {
        clickWaitRestart(menu.returnToTitleCoordinate, 2000)
      }
    case menu @ MenuType.TestingMenuNotSettingsTab =>
      println("In PlayingMenuNotSettingsTab!")
      clickWaitRestart(menu.settingsTabCoordinate, 4000)
    case menu @ MenuType.TestingMenuSettingsTabColorsEnabled =>
      println("In PlayingMenuSettingsTabColorsEnabled!")
      for (index <- 0 until 5)
        clickWait(menu.settingCoordinate(index), 100)
      start()
    case menu @ MenuType.TestingMenuSettingsTabColorsDisabled =>
      println("In PlayingMenuSettingsTabColorsDisabled!")
      clickWait(menu.deSelectBoardCoordinate, 400)
      Try(MainControl.start()) match {
        case Success(_) =>
          println("Game existed without any error! Hurray!")
          clickWaitRestart(menu.exitTestingCoordinate, 4000)
        case Failure(_: BoardStartsWithUnknownPieces) =>
          println("Board started with unknown pieces, let's try another!")
          clickWaitRestart(menu.exitTestingCoordinate, 4000)
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

}
