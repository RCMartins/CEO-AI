package ceo.play

import ceo.play.GameState.Board
import ceo.play.Player.{PlayerBlack, PlayerWhite}
import ceo.play.PlayerColor.{Black, White}

object PlayGame {
  val emptyBoard: Board = Vector.fill(8, 8)(None)
  val emptyGameState: GameState = GameState(emptyBoard, PlayerWhite(0), PlayerBlack(0), 1)

  def main(args: Array[String]): Unit = {
    val startingBoard = DataLoader.initialize()
    println(startingBoard)

    val movesPlayerWhite = startingBoard.getCurrentPlayerMoves
    println("movesPlayerWhite:")
    println(movesPlayerWhite.mkString("\n"))
    println()

    println("movesPlayerBlack:")
    val movesPlayerBlack = startingBoard.nextTurn.getCurrentPlayerMoves
    println(movesPlayerBlack.mkString("\n"))
  }

  def loadStartingBoard(): GameState = {
    import DataLoader.getPieceData

    val state1 =
      emptyGameState
        .placeUnit(Piece(getPieceData("Pawn1", White), BoardPos(2, 3)))
        .placeUnit(Piece(getPieceData("Pawn1", Black), BoardPos(1, 3)))
        .placeUnit(Piece(getPieceData("Pawn1", Black), BoardPos(1, 4)))
    state1
  }

}
