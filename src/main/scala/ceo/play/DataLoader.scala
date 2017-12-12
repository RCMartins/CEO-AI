package ceo.play

import java.io.{BufferedReader, File, FileFilter, StringReader}

import ceo.play.PlayerColor.{Black, White}

import scala.collection.mutable
import scala.io.Source

object DataLoader {

  private val units: mutable.Map[String, PieceData] = mutable.Map[String, PieceData]()

  def getPieceData(name: String, team: PlayerColor): PieceData = units(name.head.toUpper + name.tail.toLowerCase + "_" + team)

  def main(args: Array[String]): Unit = {
    println(initialize())
  }

  def initialize(): GameState = {
    loadFiles(new File("Data/Units"))
    loadBoard(new File("Data/boardTest.ceo"))
  }

  def loadFiles(file: File): Unit = {
    if (file.isDirectory)
      file.listFiles().filter(_.getName.endsWith(".ceo")).foreach(loadFile)
    else
      new RuntimeException(s"Not a directory: $file")
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
      case 'R' => Moves.RangedDestroy(posX, posY)
    }
  }

  def loadPowers(powersStr: List[String]): List[Powers] = {
    powersStr.map {
      case str if str.startsWith("Promotes ") =>
        Powers.PromoteTo(str.drop("Promotes ".length))
      case str if str.startsWith("DeathMoraleLost ") =>
        Powers.DeathMoraleLost(str.drop("DeathMoraleLost ".length).toInt)
      case str if str.startsWith("Immune ") =>
        Powers.Immune(str.drop("Immune ".length).split(" ").toList)
      case str if str.startsWith("DestroyedBy ") =>
        Powers.DestroyedBy(str.drop("DestroyedBy ".length).split(" ").toList)
      case "KingCastling" =>
        Powers.KingCastling
      case "SuicideOnKill" =>
        Powers.SuicideOnKill
      case str =>
        throw new Exception("Unknown Power: " + str)
    }
  }

  def loadBoard(file: File): GameState = {
    val lines = Source.fromFile(file).getLines.toVector
    if (lines.length < 8)
      throw new Exception("Board has less than 8 lines...")

    var gameState = PlayGame.emptyGameState
    for (row <- 0 until 8) {
      val line = lines(row).replaceAll("""\s+""", " ")
      val unitNames = line.split(" ")
      for ((unitName, column) <- unitNames.zipWithIndex) {
        if (unitName.contains("_")) {
          val List(name, team) = unitName.split("_").toList
          val pieceData = getPieceData(name, PlayerColor(team))
          gameState = gameState.placeUnit(Piece(pieceData, BoardPos(row, column)))
        }
      }
    }

    gameState
  }
}
