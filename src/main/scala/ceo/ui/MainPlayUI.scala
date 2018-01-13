package ceo.ui

import ceo.control.MainControl
import ceo.image.ImageLoader

object MainPlayUI {

  def main(args: Array[String]): Unit = {
    ImageLoader.initialize()
    //    MainControl.start() // to test it
    start()
  }

  def start(): Unit = {
    new PlayUI()
  }

}
