package ceo.menu

import java.awt.image.BufferedImage

import ceo.control.MainControl
import ceo.menu.MenuType.{Coordinate, at, testPixel}

sealed trait MenuType {

  def isInThisMenu(pixels: Array[Int]): Boolean

}

sealed trait InGameMenuType extends MenuType {

  def isInThisMenu(pixels: Array[Int]): Boolean = {
    testPixel(at(pixels, 378, 4), 0, 0, 0) && // PlayingMatch
      testPixel(at(pixels, 517, 3), 0, 0, 0) && // PlayingMatch
      testPixel(at(pixels, 617, 4), 0, 0, 0) && // PlayingMatch
      testPixel(at(pixels, 284, 643), 0, 0, 0) && // PlayingMatch
      testPixel(at(pixels, 333, 642), 0, 0, 0) // PlayingMatch
  }

  val deSelectBoardCoordinate: Coordinate = (127, 293)

  val settingsTabCoordinate: Coordinate = (338, 588)

  val unitsTakenTabCoordinate: Coordinate = (284, 587)

  val chatCoordinate: Coordinate = (775, 625)

  /** settingIndex: [0, 6[ */
  def settingCoordinate(settingIndex: Int): Coordinate = (395 + settingIndex * 60, 610)

  def notInSettingsTab(pixels: Array[Int]): Boolean =
    !testPixel(at(pixels, 338, 588), 0, 0, 0)

  def inSettingsTabColorsEnabled(pixels: Array[Int]): Boolean =
    testPixel(at(pixels, 338, 588), 0, 0, 0) &&
      testPixel(at(pixels, 390, 604), 206, 13, 13)

  def inSettingsTabColorsDisabled(pixels: Array[Int]): Boolean =
    testPixel(at(pixels, 338, 588), 0, 0, 0) &&
      !testPixel(at(pixels, 390, 604), 206, 13, 13)

  def isWhitePlayerAtBottom(pixels: Array[Int]): Boolean =
    testPixel(at(pixels, 307, 624), 255, 255, 255) &&
      testPixel(at(pixels, 298, 628), 91, 91, 91) &&
      testPixel(at(pixels, 328, 635), 61, 61, 61)
}

sealed trait OkMenu extends MenuType {
  def okCoordinate: (Coordinate, Int)
}

object MenuType {

  type Coordinate = (Int, Int)

  private val allMenus: List[MenuType] = List(WelcomeContinueMenu, MainMenuNoOpenBox, BeginCombatMenu,
    MainMenuCanOpenBox, OpenBoxMenu, OpenBoxConfirm,
    MainMenuHasLandmarks, LandmarksMenu,
    EditArmyMenu, PlayingTesting, SelectTestMethod,
    TodaysChallengeWindow, PlayingChallenge, ChallengeVictoryWindow,
    FindingPracticePartner, PlayingInMultiplayer, MultiplayerGameOverScreen,
    SearchingForOpponent, RankUpMenu
  )

  def menuMatch(image: BufferedImage, f: Array[Int] => Boolean): Boolean = {
    f(image.getRGB(0, 0, MainControl.sizeX, MainControl.sizeY, null, 0, MainControl.sizeX))
  }

  def findMenu(image: BufferedImage): Option[MenuType] = {
    val pixels: Array[Int] = image.getRGB(0, 0, MainControl.sizeX, MainControl.sizeY, null, 0, MainControl.sizeX)

    val possibleMenus =
      allMenus.collect { case menu if menu.isInThisMenu(pixels) => menu }
    if (possibleMenus.lengthCompare(1) > 0) {
      Console.err.println("Multiple possible menus:")
      Console.err.println(possibleMenus)
    }
    possibleMenus.headOption
  }

  def testPixel(pixel: Int, red: Int, green: Int, blue: Int, debug: Boolean = false): Boolean = {
    val redPart = (pixel & 0x00FF0000) >> 16
    val greenPart = (pixel & 0x0000FF00) >> 8
    val bluePart = (pixel & 0x000000FF) >> 0
    if (debug)
      println(s"($pixel) ($red, $redPart), ($green, $greenPart), ($blue, $bluePart)")
    red == redPart && green == greenPart && blue == bluePart
  }

  def at(pixels: Array[Int], x: Int, y: Int): Int = {
    pixels(x + y * MainControl.sizeX)
  }

  case object MainMenuNoOpenBox extends MenuType {
    def isInThisMenu(pixels: Array[Int]): Boolean = {
      testPixel(at(pixels, 28, 578), 82, 82, 0) &&
        !testPixel(at(pixels, 100, 375), 255, 218, 1) &&
        !testPixel(at(pixels, 389, 557), 255, 174, 45)
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

  case object OpenBoxConfirm extends OkMenu {
    def isInThisMenu(pixels: Array[Int]): Boolean = {
      testPixel(at(pixels, 30, 419), 122, 205, 122) &&
        testPixel(at(pixels, 537, 367), 15, 15, 15) &&
        testPixel(at(pixels, 205, 628), 170, 195, 219)
    }

    val okCoordinate: (Coordinate, Int) = ((500, 398), 2000)
  }

  case object BeginCombatMenu extends MenuType {
    def isInThisMenu(pixels: Array[Int]): Boolean = {
      testPixel(at(pixels, 28, 578), 164, 164, 122) &&
        testPixel(at(pixels, 159, 437), 0, 0, 0) &&
        testPixel(at(pixels, 522, 240), 3, 56, 158)
    }

    val exitCoordinate: Coordinate = (498, 498)

    val challengeButtonCoordinate: Coordinate = (281, 353)

    val rankedButtonCoordinate: Coordinate = (505, 353)

    val casualButtonCoordinate: Coordinate = (719, 353)
  }

  case object EditArmyMenu extends MenuType {

    def isInThisMenu(pixels: Array[Int]): Boolean = {
      testPixel(at(pixels, 626, 30), 0, 0, 0) &&
        testPixel(at(pixels, 674, 170), 0, 0, 0) &&
        testPixel(at(pixels, 952, 498), 92, 92, 92) &&
        testPixel(at(pixels, 942, 537), 27, 27, 27) &&
        testPixel(at(pixels, 571, 4), 189, 35, 35)
    }

    val returnToTitleCoordinate: Coordinate = (815, 38)

    /** army number [1, 16] */
    def selectArmyCoordinate(armyNumber: Int): Coordinate = (236 + armyNumber * 42, 624)

    val randomArmyCoordinate: Coordinate = (274, 411)

    val saveArmyCoordinate: Coordinate = (811, 60)

    val testArmyCoordinate: Coordinate = (760, 284)
  }

  case object OpenBoxMenu extends MenuType {
    def isInThisMenu(pixels: Array[Int]): Boolean = {
      testPixel(at(pixels, 297, 170), 0, 0, 0) &&
        testPixel(at(pixels, 701, 530), 0, 0, 0) &&
        testPixel(at(pixels, 499, 345), 224, 99, 99) &&
        testPixel(at(pixels, 606, 220), 255, 189, 189)
    }

    val chooseBoxCoordinate: Coordinate = (500, 345)

    val confirmCoordinate: Coordinate = (500, 398)
  }

  case object PlayingTesting extends InGameMenuType {
    override def isInThisMenu(pixels: Array[Int]): Boolean = {
      super.isInThisMenu(pixels) &&
        testPixel(at(pixels, 957, 154), 255, 255, 0) && // army testing side menu
        testPixel(at(pixels, 803, 178), 0, 204, 0) // army testing side menu
    }

    val exitPlayingCoordinate: Coordinate = (871, 277)

    val resetTestingCoordinate: Coordinate = (803, 178)
  }

  case object PlayingChallenge extends InGameMenuType {
    override def isInThisMenu(pixels: Array[Int]): Boolean = {
      super.isInThisMenu(pixels) &&
        testPixel(at(pixels, 801, 162), 192, 33, 33) && // challenge side menu
        testPixel(at(pixels, 797, 285), 0, 204, 0) // challenge side menu
    }

    val resetChallengeCoordinate: Coordinate = (850, 282)

    val exitPlayingCoordinate: Coordinate = (936, 282)
  }

  case object TodaysChallengeWindow extends MenuType {
    def isInThisMenu(pixels: Array[Int]): Boolean = {
      testPixel(at(pixels, 922, 294), 248, 136, 136) &&
        testPixel(at(pixels, 871, 533), 208, 147, 188) &&
        testPixel(at(pixels, 60, 561), 81, 51, 95)
    }

    def challengeWon(pixels: Array[Int]): Boolean =
      testPixel(at(pixels, 567, 584), 136, 202, 218) &&
        testPixel(at(pixels, 643, 584), 11, 14, 15) &&
        testPixel(at(pixels, 796, 584), 120, 150, 158)

    val claimPrizeCoordinate: Coordinate = (323, 537)

    val claimPrizeOkButtonCoordinate: Coordinate = (500, 398)

    val playChallengeCoordinate: Coordinate = (678, 538)

    val exitCoordinate: Coordinate = (500, 617)
  }

  case object ChallengeVictoryWindow extends MenuType {
    def isInThisMenu(pixels: Array[Int]): Boolean = {
      testPixel(at(pixels, 404, 358), 255, 11, 11) &&
        testPixel(at(pixels, 532, 369), 128, 50, 50) &&
        testPixel(at(pixels, 475, 295), 175, 124, 0)
    }

    val okButtonCoordinate: Coordinate = (500, 398)
  }

  case object FindingPracticePartner extends MenuType {
    def isInThisMenu(pixels: Array[Int]): Boolean = {
      testPixel(at(pixels, 411, 226), 105, 84, 119) &&
        testPixel(at(pixels, 512, 226), 66, 55, 74) &&
        testPixel(at(pixels, 571, 229), 92, 72, 105) &&
        testPixel(at(pixels, 523, 485), 133, 133, 133)
    }

    val cancelButtonCoordinate: Coordinate = (502, 480)
  }

  case object PlayingInMultiplayer extends InGameMenuType {
    override def isInThisMenu(pixels: Array[Int]): Boolean = {
      super.isInThisMenu(pixels) &&
        (testPixel(at(pixels, 902, 95), 116, 115, 115) || testPixel(at(pixels, 902, 96), 116, 116, 115)) &&
        testPixel(at(pixels, 880, 260), 112, 112, 112) &&
        testPixel(at(pixels, 984, 616), 85, 136, 136)
    }

    val surrenderCoordinate: Coordinate = (803, 219)

    def inBlitzGame(pixels: Array[Int]): Boolean =
      testPixel(at(pixels, 870, 203), 20, 15, 78) &&
        testPixel(at(pixels, 881, 233), 75, 42, 40)
  }

  case object MultiplayerGameOverScreen extends MenuType {
    override def isInThisMenu(pixels: Array[Int]): Boolean = {
      testPixel(at(pixels, 234, 545), 76, 48, 240) &&
        testPixel(at(pixels, 498, 544), 246, 147, 90) &&
        testPixel(at(pixels, 849, 544), 138, 56, 114)
    }

    val returnToTitleCoordinate: Coordinate = (220, 586)

    val playAgainCoordinate: Coordinate = (493, 586)
  }

  case object SelectTestMethod extends MenuType {
    override def isInThisMenu(pixels: Array[Int]): Boolean = {
      testPixel(at(pixels, 412, 86), 202, 202, 202) &&
        testPixel(at(pixels, 475, 89), 136, 136, 136) &&
        testPixel(at(pixels, 605, 90), 96, 96, 96)
    }

    val startCoordinate: Coordinate = (500, 500)

    val cancelCoordinate: Coordinate = (500, 550)
  }

  case object RankUpMenu extends OkMenu {
    override def isInThisMenu(pixels: Array[Int]): Boolean = {
      testPixel(at(pixels, 455, 223), 39, 39, 39) &&
        testPixel(at(pixels, 470, 223), 189, 189, 189) &&
        testPixel(at(pixels, 481, 231), 56, 56, 56) &&
        testPixel(at(pixels, 517, 222), 224, 224, 224) &&
        testPixel(at(pixels, 532, 231), 191, 191, 191) &&
        testPixel(at(pixels, 499, 456), 141, 89, 0)
    }

    val okCoordinate: (Coordinate, Int) = ((499, 456), 2000)
  }

  case object SearchingForOpponent extends MenuType {
    override def isInThisMenu(pixels: Array[Int]): Boolean = {
      testPixel(at(pixels, 414, 227), 83, 54, 103) &&
        testPixel(at(pixels, 449, 230), 24, 16, 30) &&
        testPixel(at(pixels, 484, 231), 144, 97, 177) &&
        testPixel(at(pixels, 589, 226), 72, 61, 79) &&
        testPixel(at(pixels, 501, 479), 55, 55, 55)
    }

    val cancelCoordinate: Coordinate = (501, 479)
  }

  case object WelcomeContinueMenu extends MenuType {
    override def isInThisMenu(pixels: Array[Int]): Boolean = {
      testPixel(at(pixels, 463, 147), 51, 51, 51) &&
        testPixel(at(pixels, 610, 167), 120, 120, 120) &&
        testPixel(at(pixels, 484, 340), 251, 191, 173) &&
        testPixel(at(pixels, 510, 338), 168, 164, 160)
    }

    val continueCoordinate: Coordinate = (494, 507)

    val skipCoordinate: Coordinate = (990, 640)
  }

  case object MainMenuHasLandmarks extends MenuType {
    override def isInThisMenu(pixels: Array[Int]): Boolean = {
      testPixel(at(pixels, 356, 557), 249, 159, 159) && // has landmark
        testPixel(at(pixels, 389, 557), 255, 174, 45) && // has landmark
        testPixel(at(pixels, 451, 560), 199, 169, 94) && // has landmark
        !testPixel(at(pixels, 100, 375), 255, 218, 1)
    }

    val viewLandmarksCoordinate: Coordinate = (412, 476)
  }

  case object LandmarksMenu extends MenuType {
    override def isInThisMenu(pixels: Array[Int]): Boolean = {
      testPixel(at(pixels, 56, 474), 255, 255, 0) &&
        testPixel(at(pixels, 56, 505), 0, 255, 0) &&
        testPixel(at(pixels, 56, 539), 0, 204, 255) &&
        testPixel(at(pixels, 56, 569), 255, 0, 255) &&
        testPixel(at(pixels, 56, 603), 255, 0, 0)
    }

    /** index between 0 and 3 */
    def claimButtonCoordinate(index: Int): Coordinate = (717, 174 + index * 114)

    val okCoordinate: Coordinate = (500, 382)

    val exitCoordinate: Coordinate = (936, 609)
  }

}
