package ceo.play

import java.awt.Toolkit

import scala.util.Random

object Util {

  val random: Random = new Random(3)

  @inline final val ValueOfStateMaxValue = 1e9.toInt

  def Beep(): Unit = Toolkit.getDefaultToolkit.beep()
}
