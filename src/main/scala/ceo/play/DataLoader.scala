package ceo.play

import java.io.{BufferedReader, File, StringReader}

import ceo.play.PlayerTeam.{Black, White}

import scala.collection.mutable
import scala.io.Source
import scala.util.{Failure, Success, Try}

object DataLoader {

  private val units: mutable.Map[String, PieceData] = mutable.Map[String, PieceData]()
  private var piecesToCheck: List[String] = List[String]()

  def getPieceData(name: String, team: PlayerTeam): PieceData = units(name + "_" + team)

  def main(args: Array[String]): Unit = {
    println(initialize())
  }

  def initialize(boardToStart: String = "Data/boardTest.ceo"): GameState = {
    loadFiles(new File("Data/Units"))
    val board = loadBoard(new File(boardToStart))

    val unknownPieces =
      piecesToCheck.flatMap(piece => if (Try(getPieceData(piece, White)).isFailure) Some(piece) else None) ++
        piecesToCheck.flatMap(piece => if (Try(getPieceData(piece, Black)).isFailure) Some(piece) else None)

    if (unknownPieces.nonEmpty) {
      System.err.println("Unknown pieces:")
      System.err.println(unknownPieces.distinct.mkString("\n"))
      ???
    } else
      board
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
      br.readLine()
      val movesStr = Stream.continually(readLine(false)).takeWhile(!_.startsWith("-" * 3)).toList
      val powersStr = Stream.continually(readLine()).takeWhile(!_.startsWith("-" * 20)).toList

      val powers = loadPowers(powersStr)
      val movesBottomTop = loadMoves(movesStr, powers)
      val movesTopBottom = loadMoves(movesStr.reverse, powers)

      val name_white = name + "_" + White
      val name_black = name + "_" + Black
      Seq(
        PieceData(name_white, morale.toInt, movesBottomTop, powers, White),
        PieceData(name_black, morale.toInt, movesTopBottom, powers, Black)
      )
    }
  }

  def loadMoves(movesStr: List[String], powers: List[Powers]): List[Moves] = {
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
      case 'T' => Moves.MoveUnblockable(posX, posY)
      case 'A' => Moves.Attack(posX, posY)
      case 'J' => Moves.MoveOrAttackUnblockable(posX, posY)
      case 'S' => Moves.MoveOrAttackOrSwapAlly(posX, posY)
      case 'R' => Moves.RangedDestroy(posX, posY)
      case '1' | '2' | '3' =>
        powers.collectFirst { case move: MovePower if move.letterOfMove == char => move } match {
          case None =>
            throw new Exception("Unknown 'MovePower' letter: " + char)
          case Some(movePower) =>
            movePower.createMove(posX, posY)
        }
    }
  }

  def loadPowers(powersStr: List[String]): List[Powers] = {
    powersStr.map {
      case "KingCastling" =>
        Powers.KingCastling
      case "SuicideOnKill" =>
        Powers.SuicideOnKill
      case "GhostMovement" =>
        Powers.GhostMovement
      case str if str.startsWith("DummyNothingPower ") =>
        Powers.DummyNothingPower(str.drop("DummyNothingPower ".length).head)
      case str if str.startsWith("PromotesTo ") =>
        val pieceToCkeck = str.drop("PromotesTo ".length)
        piecesToCheck = pieceToCkeck :: piecesToCheck
        Powers.PromoteTo(pieceToCkeck)
      case str if str.startsWith("LoseMoraleOnDeath ") =>
        Powers.LoseMoraleOnDeath(str.drop("LoseMoraleOnDeath ".length).toInt)
      case str if str.startsWith("GainMoraleOnKill ") =>
        Powers.GainMoraleOnKill(str.drop("GainMoraleOnKill ".length).toInt)
      case str if str.startsWith("DecayAfterTurn ") =>
        val List(turnStarts, moralePerTurn) = str.drop("DecayAfterTurn ".length).split(" ").toList
        Powers.DecayAfterTurn(turnStarts.toInt, moralePerTurn.toInt)
      case str if str.startsWith("Immune ") =>
        Powers.Immune(str.drop("Immune ".length).split(" ").toList)
      case str if str.startsWith("DestroyedBy ") =>
        Powers.DestroyedBy(str.drop("DestroyedBy ".length).split(" ").toList)
      // Move Powers:
      case str if str.startsWith("MagicDestroy ") && str.length == "MagicDestroy ".length + 1 =>
        Powers.MagicDestroyMovePower(str.takeRight(1).head)
      case str if str.startsWith("RangedPetrify ") =>
        val List(letterStr, duration) = str.drop("RangedPetrify ".length).split(" ").toList
        Powers.RangedPetrifyMovePower(letterStr.head, duration.toInt)
      case str if str.startsWith("TaurusRush ") =>
        val List(letterStr, maxDistance) = str.drop("TaurusRush ".length).split(" ").toList
        Powers.TaurusRushMovePower(letterStr.head, maxDistance.toInt)
      case str if str.startsWith("MagicTransformIntoAllyUnit ") =>
        val List(letterStr, moraleCost, allyUnitName) = str.drop("MagicTransformIntoAllyUnit ".length).split(" ").toList
        piecesToCheck = allyUnitName :: piecesToCheck
        Powers.TransformIntoAllyMovePower(letterStr.head, moraleCost.toInt, allyUnitName)
      case str =>
        throw new Exception("Unknown Power: " + str)
    }
  }

  def loadBoard(file: File): GameState = {
    val lines = Source.fromFile(file).getLines.toVector

    var gameState = PlayGame.emptyGameState
    for (row <- lines.indices) {
      val line = lines(row).replaceAll("""\s+""", " ")
      val unitNames = line.split(" ")
      for ((unitName, column) <- unitNames.zipWithIndex) {
        if (unitName.length > 1) {
          val List(name, team) = unitName.split("_").toList
          Try(getPieceData(name, PlayerTeam(team))) match {
            case Failure(_) =>
              piecesToCheck = name :: piecesToCheck
            case Success(pieceData) =>
              gameState = gameState.placePiece(Piece(pieceData, BoardPos(row, column)))
          }
        }
      }
    }

    gameState
  }
}
