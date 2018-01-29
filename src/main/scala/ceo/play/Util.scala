package ceo.play

import java.awt.Toolkit

import scala.util.Random

object Util {

  final val random: Random = new Random(3)

  @inline final val ValueOfStateMaxValue = 1e9.toInt

  def Beep(): Unit = Toolkit.getDefaultToolkit.beep()

  def Beep5(): Unit = {
    def b = {
      Toolkit.getDefaultToolkit.beep()
      Thread.sleep(300)
    }

    (1 to 5).foreach(_ => b)
  }
}
