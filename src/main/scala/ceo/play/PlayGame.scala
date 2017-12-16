package ceo.play

import ceo.play.Player.{PlayerBlack, PlayerWhite}

import scala.util.Random

object PlayGame {

  val emptyGameState: GameState = GameState(EmptyBoard, PlayerWhite(0), PlayerBlack(0), 1, Nil)

  def main(args: Array[String]): Unit = {
    val startingState = DataLoader.initialize("Data/boardTest.ceo")
    //    println(startingState)

    //    val movesPlayerWhite = startingState.getCurrentPlayerMoves
    //    println("movesPlayerWhite:")
    //    println(movesPlayerWhite.mkString("\n"))
    //    println()
    //
    //    val (bestMove, after) = playBestMove(startingState)
    //    println(bestMove)
    //    println(after)
    //
    //    println("movesPlayerBlack:")
    //    val movesPlayerBlack = after.nextTurn.getCurrentPlayerMoves
    //    println(movesPlayerBlack.mkString("\n"))

    println(playFullGame(startingState))
  }

  def playBestMove(startingState: GameState): Option[GameState] = {
    val currentPlayer = startingState.getCurrentPlayer
    val moves = startingState.getCurrentPlayerMoves
    val nextStates =
      moves.map(move => startingState.playPlayerMove(move))

    val statesSorted =
      nextStates
        .map(after => (after, GameState.compare(startingState, after, currentPlayer.team)))
        .sortBy(-_._2)

    if (statesSorted.isEmpty)
      None
    else {
      val bestMoves =
        statesSorted.takeWhile(_._2 == statesSorted.head._2)
      Some(bestMoves(Random.nextInt(bestMoves.length))._1)
    }
  }

  def playFullGame(startingState: GameState): GameState = {
    println(startingState)
    println(startingState.movesHistory.mkString("\n"))
    playBestMove(startingState) match {
      case None => ???
      case Some(stateAfter) =>
        if (stateAfter.winner.isDefined)
          stateAfter
        else
          playFullGame(stateAfter)
    }
  }

}
