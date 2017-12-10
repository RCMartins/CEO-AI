package ceo.play

import ceo.play.GameState.Board
import ceo.play.Player.{PlayerBlack, PlayerWhite}
import ceo.play.PlayerColor.{Black, White}

object PlayGame {
  val emptyBoard: Board = Vector.fill(8, 8)(None)
  val emptyGameState: GameState = GameState(emptyBoard, PlayerWhite(0), PlayerBlack(0), 1)

  def main(args: Array[String]): Unit = {
    DataLoader.main(Array.empty)
    val startingBoard = loadStartingBoard()
    println(startingBoard)

    val movesPlayerWhite = startingBoard.getCurrentPlayerMoves
    println(movesPlayerWhite.mkString("\n"))
    println()

    val movesPlayerBlack = startingBoard.nextTurn.getCurrentPlayerMoves
    println(movesPlayerBlack.mkString("\n"))
  }

  def loadStartingBoard(): GameState = {
    import DataLoader.getUnit

    val state1 =
      emptyGameState
        .placeUnit(Piece(getUnit("Pawn1", White), BoardPos(2, 3), White))
        .placeUnit(Piece(getUnit("Pawn1", Black), BoardPos(1, 3), Black))
        .placeUnit(Piece(getUnit("Pawn1", Black), BoardPos(1, 4), Black))
    state1
  }

}
