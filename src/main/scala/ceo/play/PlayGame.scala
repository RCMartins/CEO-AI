package ceo.play

import ceo.play.Player.{PlayerBlack, PlayerWhite}
import ceo.play.PlayerTeam.{Black, White}

object PlayGame {

  val emptyGameState: GameState = GameState(EmptyBoard, PlayerWhite(0), PlayerBlack(0), 1, Nil)

  def main(args: Array[String]): Unit = {
    val time = System.currentTimeMillis()
    val startingState = DataLoader.initialize("Data/boardStandard.ceo")
    //    playSomeMatches(startingState, Strategy.oneMoveStrategy, Strategy.oneMoveStrategy)
    //        playSomeMatches(startingState, Strategy.MinMaxStrategy(3), Strategy.oneMoveStrategy)
    playSomeMatches(startingState, Strategy.MinMaxStrategyPar(3), Strategy.MinMaxStrategyPar(2))
    //    playSomeMatches(startingState, Strategy.MinMaxStrategy(3), Strategy.oneMoveStrategy)
    //    play(startingState, Strategy.MinMaxStrategy(3), Strategy.oneMoveStrategy)
    //        play(startingState, Strategy.MinMaxStrategy(1), Strategy.oneMoveStrategy)
    println(s"Total time: ${System.currentTimeMillis() - time}")
  }

  def playSomeMatches(startingState: GameState, playerWhiteStrategy: Strategy, playerBlackStrategy: Strategy): Unit = {
    println(startingState)
    val playSomeMatches: Seq[(GameState, PlayerTeam)] =
      (1 to 15)
        .map(index => {
          if (index % 1 == 0) println(index + "...")
          playFullGame(startingState, playerWhiteStrategy, playerBlackStrategy)
        })
        .map(finalState => (finalState, finalState.winner.get))

    val minWinState = playSomeMatches.minBy(_._1.currentTurn)._1
    val maxWinState = playSomeMatches.maxBy(_._1.currentTurn)._1
    val whiteWins = playSomeMatches.count(_._2 == White)
    val blackWins = playSomeMatches.count(_._2 == Black)
    println(s"White: $whiteWins, Black: $blackWins")
    println(s"Min turn game: ${minWinState.currentTurn}\n$minWinState\n${minWinState.movesHistory.mkString("\n")}\n")
    println(s"Max turn game: ${maxWinState.currentTurn}\n$maxWinState\n${maxWinState.movesHistory.mkString("\n")}")

//    println()
//    println(minWinState.movesHistory.reverse.scanLeft(startingState)((state, move) => state.playPlayerMove(move)).mkString("\n"))
  }

  def play(startingState: GameState, playerWhiteStrategy: Strategy, playerBlackStrategy: Strategy): Unit = {
    val finalState = playFullGame(startingState, playerWhiteStrategy, playerBlackStrategy)
    println(finalState)
    println(finalState.movesHistory.mkString("\n"))
  }

  def playFullGame(startingState: GameState, playerWhiteStrategy: Strategy, playerBlackStrategy: Strategy): GameState = {
    //    println(startingState)
    //    println(startingState.movesHistory.mkString("\n"))
    val strategy = startingState.getCurrentPlayer.team.chooseWhiteBlack(playerWhiteStrategy, playerBlackStrategy)
    strategy.chooseMove(startingState) match {
      case None =>
        println(startingState)
        println(startingState.movesHistory.mkString("\n"))
        ???
      case Some(stateAfter) =>
//        if (stateAfter.currentTurn > 50) {
//          println(stateAfter)
//          println(stateAfter.movesHistory.mkString("\n"))
//        } else
          print(stateAfter.currentTurn + " ")
        if (stateAfter.winner.isDefined) {
          println()
          stateAfter
        } else
          playFullGame(stateAfter, playerWhiteStrategy, playerBlackStrategy)
    }
  }

}
