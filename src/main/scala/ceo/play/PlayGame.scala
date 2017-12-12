package ceo.play

import ceo.play.Player.{PlayerBlack, PlayerWhite}

object PlayGame {

  val emptyGameState: GameState = GameState(EmptyBoard, PlayerWhite(0), PlayerBlack(0), 1)

  def main(args: Array[String]): Unit = {
    val startingState = DataLoader.initialize()
    println(startingState)

    val movesPlayerWhite = startingState.getCurrentPlayerMoves
    println("movesPlayerWhite:")
    println(movesPlayerWhite.mkString("\n"))
    println()

    val (bestMove, after) = playBestMove(startingState)
    println(bestMove)
    println(after)

    println("movesPlayerBlack:")
    val movesPlayerBlack = after.nextTurn.getCurrentPlayerMoves
    println(movesPlayerBlack.mkString("\n"))
  }

  def playBestMove(startingState: GameState): (PlayerMove, GameState) = {
    val currentPlayer = startingState.getCurrentPlayer
    val moves = startingState.getCurrentPlayerMoves
    val nextStates =
      moves.map(move => startingState.playPlayerMove(move))

   nextStates.maxBy(after => GameState.compare(startingState, after, currentPlayer.team))

    ???
  }

}
