package ceo

import java.awt.image.BufferedImage
import java.io.File
import java.io.IOException
import javax.imageio.ImageIO

object FillKnownPieces {

  def reload(): Unit = {
    val folder: File = new File("KnownPieces")
    val listOfFiles: Array[File] = folder.listFiles()
    if (listOfFiles.length > 0)
      return
    println("RELOADING KNOWN PIECES!")

//    {
//      val fileName: String = "PRINTS/print_test2.png"
//      val names: Array[Array[String]] = Array(
//        Array("Knight1_black",
//          "King_black",
//          "Bishop2_black",
//          "Ranger2_black",
//          "Queen1_black",
//          "Pyromancer2_black",
//          "Wisp2_black",
//          "Rook1_black"),
//        Array("Spearman1_black",
//          "Pawn1_black",
//          "Pawn1_black",
//          "Slime1_black",
//          "Archer1_black",
//          "Pawn1_black",
//          "Pawn1_black",
//          "Phantasm1_black"),
//        Array(),
//        Array(),
//        Array(),
//        Array(),
//        Array("Pawn1_white", "Pawn1_white", "Militia1_white"),
//        Array("Paladin1_white",
//          "Ranger2_white",
//          "Knight1_white",
//          "Dragon1_white",
//          "Ranger2_white",
//          "King_white",
//          "Queen1_white",
//          "Wisp2_white")
//      )
//      reloadKnownPieces(fileName, names)
//    }

    {
      val fileName: String = "PRINTS/print_test1.png"
      val names: Array[String] = Array(
        "Princess1_black",
        "Dummy1_white",
        "Militia1_white",
        "Bomber1_white",
        "Samurai1_white",
        "Basilisk1_white",
        "Royalguard1_white",
        "Spearman1_white",
        "Swordsman1_white",
        "King_white",
        "Phoenix1_white",
        "Dragon1_white",
        "Enchantress1_white",
        "Guardian1_white",
        "Gemini1_white",
        "Harpy1_white",
        "Paladin1_white"
      )
      reloadKnownPieces(fileName, names)
    }
  }

  private def reloadKnownPieces(fileName: String, names: Array[Array[String]]): Unit = {
    val imageIn = ImageIO.read(new File(fileName))
    val black = Main.findSquareBorders(imageIn)
    Main.updateSquareSize(imageIn)
    for (y <- 1 to names.length; x <- 1 to names(y - 1).length) {
      val name: String = names(y - 1)(x - 1)
      val targetPath: String = "KnownPieces/" + name + ".png"
      if (!new File(targetPath).exists()) {
        Main.getPieceFromBlack(black, x, y)
        Main.copyImage("SQUARES/" + x + "-" + y + ".png", targetPath)
      }
    }
  }

  private def reloadKnownPieces(fileName: String, names: Array[String]): Unit = {
    val imageIn = ImageIO.read(new File(fileName))
    val black = Main.findSquareBorders(imageIn)
    var k: Int = 0

    for (y <- 1 to 8; x <- 1 to 8) {
      val piece: Piece = Main.getPieceFromBlack(black, x, y)
      if (piece.blackOpt.isDefined) {
        val name: String = names(k)
        k += 1
        val targetPath: String = "KnownPieces/" + name + ".png"
        if (!new File(targetPath).exists()) {
          Main.copyImage("SQUARES/" + x + "-" + y + ".png", targetPath)
        }
      }
    }
  }

}
