package ceo.control

import java.awt.Robot
import java.awt.event.InputEvent

object MouseControl {
  val robot = new Robot()

  def moveMouse(x: Int, y: Int): Unit = {
    robot.mouseMove(x, y)
  }

  def mouseDown(): Unit = {
    robot.mousePress(InputEvent.BUTTON1_DOWN_MASK)
  }

  def mouseUp(): Unit = {
    robot.mouseRelease(InputEvent.BUTTON1_DOWN_MASK)
  }

}
