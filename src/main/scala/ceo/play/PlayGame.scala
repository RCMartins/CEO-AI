package ceo.play

import ceo.play.Player.{PlayerBlack, PlayerWhite}
import ceo.play.PlayerTeam.{Black, White}

object PlayGame {

  val emptyGameState: GameState = GameState(EmptyBoard, PlayerWhite(0), PlayerBlack(0), 1, Nil)

  def main(args: Array[String]): Unit = {
    val time = System.currentTimeMillis()
//    val startingState = DataLoader.initialize("Data/boardStandard.ceo")
    val startingState = DataLoader.initialize("PRINTS/challenge-17-12-2017.ceo")
    //    playSomeMatches(startingState, Strategy.oneMoveStrategy, Strategy.oneMoveStrategy)
    //        playSomeMatches(startingState, Strategy.MinMaxStrategy(3), Strategy.oneMoveStrategy)
    //    playSomeMatches(startingState, Strategy.MinMaxStrategyPar(3), Strategy.MinMaxStrategyPar(2))
    //    playSomeMatches(startingState, Strategy.MinMaxStrategy(3), Strategy.oneMoveStrategy)
        play(startingState, Strategy.MinMaxStrategy(3),Strategy.MinMaxStrategy(3))
//    playAgainstExternalInput(startingState, Strategy.MinMaxStrategy(3))
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
          stateAfter
        } else
          playFullGame(stateAfter, playerWhiteStrategy, playerBlackStrategy)
    }
  }

  def playAgainstExternalInput(startingState: GameState, strategy: Strategy): GameState = {
    def machineMove(state: GameState, history: List[GameState]): GameState = {
      strategy.chooseMove(state) match {
        case None =>
          println(state)
          println(state.movesHistory.mkString("\n"))
          ???
        case Some(stateAfter) =>
          println(stateAfter)
          println("Move made: " + stateAfter.movesHistory.head + "\n")
          if (stateAfter.winner.isDefined) {
            println("GAME OVER")
            stateAfter
          } else {
            inputMove(stateAfter, state :: history)
          }
      }
    }

    def inputMove(state: GameState, history: List[GameState]): GameState = {
      playExternalInputMove(state) match {
        case Some(stateAfter) => machineMove(stateAfter, state :: history)
        case None if history.lengthCompare(2) >= 0 => inputMove(history(1), history.drop(2))
        case _ =>
          println("INVALID INPUT")
          inputMove(state, history)
      }
    }

    machineMove(startingState, Nil)
  }

  def playExternalInputMove(gameState: GameState): Option[GameState] = {
    val AInt = Char.char2int('A')

    def indexToRep(index: Int): String = {
      if (index > 26) {
        (AInt + (index / 26)).toChar.toString + (AInt + (index % 26)).toChar.toString
      } else {
        (AInt + index).toChar.toString
      }
    }

    def repToIndex(input: String): Int = {
      if (input.length == 2) {
        val v1 = Char.char2int(input.head) - AInt
        val v2 = Char.char2int(input(1)) - AInt
        v1 * 26 + v2
      } else {
        Char.char2int(input.head) - AInt
      }
    }

    val allMoves = gameState.getCurrentPlayerMoves.toVector
    val movesPretty = allMoves.zipWithIndex
      .map { case (move, index) => s"${indexToRep(index)}  -  ${move.betterHumanString}" }
      .mkString("\n")

    var inputIndex = 0
    do {
      println(movesPretty)
      val input = scala.io.StdIn.readLine()
      if (input == "UNDO")
        inputIndex = Int.MinValue
      else
        inputIndex = repToIndex(input)
    } while (inputIndex > Int.MinValue && inputIndex < 0 || inputIndex >= allMoves.length)
    if (inputIndex == Int.MinValue)
      None
    else
      Some(gameState.playPlayerMove(allMoves(inputIndex)))
  }

}
