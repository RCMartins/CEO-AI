package ceo.play

import java.awt.Toolkit

import scala.util.Random

object Util {

  final val random: Random = new Random()

  @inline final val ValueOfStateMaxValue = 1e9.toInt

  def Beep(): Unit = Toolkit.getDefaultToolkit.beep()

  def Beep5(): Unit = {
    def doBeep(): Unit = {
      Toolkit.getDefaultToolkit.beep()
      Thread.sleep(300)
    }

    (1 to 5).foreach(_ => doBeep())
  }
}
