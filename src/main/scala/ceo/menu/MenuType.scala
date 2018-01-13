package ceo.menu

import java.awt.image.BufferedImage

import ceo.control.MainControl

trait MenuType {

  def isInThisMenu(pixels: Array[Int]): Boolean

}

object MenuType {

  type Coordinate = (Int, Int)

  val all: List[MenuType] = List(MainMenuNoOpenBox, MainMenuCanOpenBox, BeginCombatMenu, OpenBoxMenu, EditArmyMenu,
    TestingMenuNotSettingsTab, TestingMenuSettingsTabColorsEnabled, TestingMenuSettingsTabColorsDisabled)

  def findMenu(image: BufferedImage): Option[MenuType] = {
    val pixels: Array[Int] = image.getRGB(0, 0, MainControl.sizeX, MainControl.sizeY, null, 0, MainControl.sizeX)

    all.find(menu => menu.isInThisMenu(pixels))
  }

  def testPixel(pixel: Int, red: Int, green: Int, blue: Int): Boolean = {
    val redPart = (pixel & 0x00FF0000) >> 16
    val greenPart = (pixel & 0x0000FF00) >> 8
    val bluePart = (pixel & 0x000000FF) >> 0
    red == redPart && green == greenPart && blue == bluePart
  }

  def at(pixels: Array[Int], x: Int, y: Int): Int = {
    pixels(x + y * MainControl.sizeX)
  }

  case object MainMenuNoOpenBox extends MenuType {
    def isInThisMenu(pixels: Array[Int]): Boolean = {
      testPixel(at(pixels, 28, 578), 82, 82, 0) &&
        !testPixel(at(pixels, 100, 375), 255, 218, 1)
    }

    val beginCombatCoordinate: Coordinate = (413, 316)

    val editArmyCoordinate: Coordinate = (579, 316)
  }

  case object MainMenuCanOpenBox extends MenuType {
    def isInThisMenu(pixels: Array[Int]): Boolean = {
      testPixel(at(pixels, 28, 578), 82, 82, 0) &&
        testPixel(at(pixels, 100, 375), 255, 218, 1)
    }

    val boxOpenCoordinate: Coordinate = (100, 375)
  }

  case object BeginCombatMenu extends MenuType {
    def isInThisMenu(pixels: Array[Int]): Boolean = {
      testPixel(at(pixels, 28, 578), 164, 164, 122) &&
        testPixel(at(pixels, 159, 437), 0, 0, 0)
    }
  }

  case object EditArmyMenu extends MenuType {

    def isInThisMenu(pixels: Array[Int]): Boolean = {
      testPixel(at(pixels, 626, 30), 0, 0, 0) &&
        testPixel(at(pixels, 674, 170), 0, 0, 0)
    }

    val returnToTitleCoordinate: Coordinate = (815, 38)

    /** army number [1, 16] */
    def selectArmyCoordinate(armyNumber: Int): Coordinate = (236 + armyNumber * 42, 624)

    val randomArmyCoordinate: Coordinate = (274, 411)

    val saveArmyCoordinate: Coordinate = (811, 60)

    val testArmyCoordinate: Coordinate = (760, 284)

    val testArmyStartCoordinate: Coordinate = (499, 505)
  }

  case object OpenBoxMenu extends MenuType {
    def isInThisMenu(pixels: Array[Int]): Boolean = {
      testPixel(at(pixels, 297, 170), 0, 0, 0) &&
        testPixel(at(pixels, 701, 530), 0, 0, 0)
    }

    val chooseBoxCoordinate: Coordinate = (500, 345)

    val confirmCoordinate: Coordinate = (500, 398)
  }

  case object TestingMenuSettingsTabColorsDisabled extends MenuType {
    def isInThisMenu(pixels: Array[Int]): Boolean = {
      testPixel(at(pixels, 378, 4), 0, 0, 0) &&
        testPixel(at(pixels, 517, 3), 0, 0, 0) &&
        testPixel(at(pixels, 617, 4), 0, 0, 0) &&
        testPixel(at(pixels, 284, 643), 0, 0, 0) &&
        testPixel(at(pixels, 333, 643), 124, 124, 123) &&
        testPixel(at(pixels, 338, 588), 0, 0, 0) &&
        !testPixel(at(pixels, 390, 604), 206, 13, 13)
    }

    val deSelectBoardCoordinate: Coordinate = (127, 293)

    val exitTestingCoordinate: Coordinate = (871, 277)
  }

  case object TestingMenuSettingsTabColorsEnabled extends MenuType {
    def isInThisMenu(pixels: Array[Int]): Boolean = {
      testPixel(at(pixels, 378, 4), 0, 0, 0) &&
        testPixel(at(pixels, 517, 3), 0, 0, 0) &&
        testPixel(at(pixels, 617, 4), 0, 0, 0) &&
        testPixel(at(pixels, 284, 643), 0, 0, 0) &&
        testPixel(at(pixels, 333, 643), 124, 124, 123) &&
        testPixel(at(pixels, 338, 588), 0, 0, 0) &&
        testPixel(at(pixels, 390, 604), 206, 13, 13)
    }

    /** settingIndex: [0, 6[ */
    def settingCoordinate(settingIndex: Int): Coordinate = (395 + settingIndex * 60, 610)
  }

  case object TestingMenuNotSettingsTab extends MenuType {
    def isInThisMenu(pixels: Array[Int]): Boolean = {
      testPixel(at(pixels, 378, 4), 0, 0, 0) &&
        testPixel(at(pixels, 517, 3), 0, 0, 0) &&
        testPixel(at(pixels, 617, 4), 0, 0, 0) &&
        testPixel(at(pixels, 284, 643), 0, 0, 0) &&
        testPixel(at(pixels, 333, 643), 124, 124, 123) &&
        !testPixel(at(pixels, 338, 588), 0, 0, 0)
    }

    val settingsTabCoordinate: Coordinate = (338, 588)
  }

}
