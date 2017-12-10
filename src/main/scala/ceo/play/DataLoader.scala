package ceo.play

import java.io.{BufferedReader, File, StringReader}

import ceo.play
import ceo.play.PlayerColor.{Black, White}

import scala.collection.mutable
import scala.io.Source

object DataLoader {

  private val units: mutable.Map[String, PieceData] = mutable.Map[String, PieceData]()

  def getUnit(name: String, team: PlayerColor): PieceData = units(name.head.toUpper + name.tail.toLowerCase + "_" + team)

  def main(args: Array[String]): Unit = {
    loadFile(new File("Data/units.ceo"))
  }

  def loadFile(file: File): Unit = {
    val br = new BufferedReader(new StringReader(Source.fromFile(file).getLines.mkString("\n")))

    def loadUnits(): Unit = {
      val gamePieces = loadUnit(br)
      gamePieces.foreach {
        gamePiece =>
          units.put(gamePiece.name, gamePiece)
      }
      if (gamePieces.nonEmpty)
        loadUnits()
    }

    loadUnits()
  }

  def loadUnit(br: BufferedReader): Seq[PieceData] = {
    def readLine(ignoreEmptyLines: Boolean = true): String = {
      val line = br.readLine()
      if (line != null && (line.isEmpty && ignoreEmptyLines || line.startsWith("//"))) readLine() else line
    }

    val firstLine = readLine()
    if (firstLine == null)
      Seq.empty
    else {
      val List(name, morale) = firstLine.split(" ").toList
      //      println((name, morale))
      br.readLine()
      val movesStr = Stream.continually(readLine(false)).takeWhile(!_.startsWith("-" * 3)).toList
      //      println(movesStr)
      val powersStr = Stream.continually(readLine()).takeWhile(!_.startsWith("-" * 20)).toList
      //      println(powersStr)

      val movesBottomTop = loadMoves(movesStr)
      val movesTopBottom = loadMoves(movesStr.reverse)
      val powers = loadPowers(powersStr)

      //      println(moves)
      //      println(powers)
      //      println()

      val name_white = name + "_" + White
      val name_black = name + "_" + Black
      Seq(
        PieceData(name_white, morale.toInt, movesBottomTop, powers, White),
        PieceData(name_black, morale.toInt, movesTopBottom, powers, Black)
      )
    }
  }

  def loadMoves(movesStr: List[String]): List[Moves] = {
    val iy = movesStr.indexWhere(_.contains('@'))
    val ix = movesStr(iy).indexWhere(_ == '@')

    for {
      (line, y) <- movesStr.zipWithIndex
      (char, x) <- line.zipWithIndex
      if char != ' ' && char != '@'
      posX = x - ix
      posY = y - iy
    } yield char match {
      case 'N' => Moves.MoveOrAttack(posX, posY)
      case 'M' => Moves.Move(posX, posY)
      case 'm' => Moves.MoveFromStart(posX, posY)
      case 'A' => Moves.Attack(posX, posY)
      case 'J' => Moves.MoveOrAttackUnblockable(posX, posY)
      case 'S' => Moves.MoveOrAttackOrSwapAlly(posX, posY)
    }
  }

  def loadPowers(powersStr: List[String]): List[Powers] = {
    powersStr.map {
      case str if str.startsWith("Promotes ") =>
        Powers.PromoteTo(str.drop("Promotes ".length))
      case "KingCastling" =>
        Powers.KingCastling
      case str if str.startsWith("DeathMoraleLost ") =>
        Powers.DeathMoraleLost(str.drop("DeathMoraleLost ".length).toInt)
      case str =>
        throw new Exception("Unknown Power: " + str)
    }
  }

}
