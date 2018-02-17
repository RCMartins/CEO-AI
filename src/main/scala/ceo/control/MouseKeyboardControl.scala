package ceo.control

import java.awt.{MouseInfo, Robot}
import java.awt.event.InputEvent

object MouseKeyboardControl {

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

  def getMousePosition: (Int, Int) = {
    val point = MouseInfo.getPointerInfo.getLocation
    (point.x, point.y)
  }

  def typeText(string: String): Unit = {
    val bytes = string.getBytes
    for (byte <- bytes) {
      var code: Int = byte
      // keycode only handles [A-Z] (which is ASCII decimal [65-90])
      if (code > 96 && code < 123)
        code -= 32
      robot.delay(50)
      robot.keyPress(code)
      robot.keyRelease(code)
    }
  }
}
