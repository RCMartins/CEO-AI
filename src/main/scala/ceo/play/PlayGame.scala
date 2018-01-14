package ceo.play

import ceo.play.PlayerTeam.{Black, White}

import scala.util.Try

object PlayGame {

  final val DEBUG_SHOW_TURNS: Boolean = false
  final val DEBUG_SHOW_CALC_TIME: Boolean = true

  val emptyGameState: GameState =
    GameState(
      Board.empty,
      Player(White, 0, Nil, Nil, 0, hasKing = false, PlayerExtraData.empty),
      Player(Black, 0, Nil, Nil, 0, hasKing = false, PlayerExtraData.empty),
      1,
      Nil,
      Nil,
      GameRunner.empty
    )

  def playSomeMatches(startingState: GameState, playerWhiteStrategy: Strategy, playerBlackStrategy: Strategy, numberOfGames: Int): Unit = {
    println(startingState)
    val playSomeMatches: Seq[(GameState, PlayerWinType)] =
      (1 to numberOfGames)
        .map(index => {
          if (index % 1 == 0) println(index + "...")
          playFullGame(startingState, playerWhiteStrategy, playerBlackStrategy)
        })
        .map(finalState => (finalState, finalState.winner))

    val minWinState = playSomeMatches.minBy(_._1.currentTurn)._1
    val maxWinState = playSomeMatches.maxBy(_._1.currentTurn)._1
    val whiteWins = playSomeMatches.count(_._2 == PlayerWinType.PlayerWhite)
    val blackWins = playSomeMatches.count(_._2 == PlayerWinType.PlayerBlack)
    val draws = playSomeMatches.count(_._2 == PlayerWinType.Draw)
    println(s"White: $whiteWins, Black: $blackWins, Draw: $draws")
    println(s"Min turn game: ${minWinState.currentTurn}\n$minWinState\n${minWinState.movesHistory.mkString("\n")}\n")
    println(s"Max turn game: ${maxWinState.currentTurn}\n$maxWinState\n${maxWinState.movesHistory.mkString("\n")}")

    //    println()
    //    println(minWinState.movesHistory.reverse.scanLeft(startingState)((state, move) => state.playPlayerMove(move)).mkString("\n"))
  }

  private def showStateWithMoves(state: GameState): Unit = {
    println(state)
    println(state.movesHistory.reverse.zipWithIndex.reverse.map { case (move, index) =>
      val turn: Double = index / 2.0 + 1
      f"$turn%3.1f | ${move.betterHumanString}"
    }.mkString("\n"))
  }

  private def showAllGameStates(startingState: GameState, movesHistory: List[PlayerMove]): Unit = {
    def show(state: GameState): Unit = {
      println(state)
      if (state.movesHistory.nonEmpty)
        println(s"${state.currentTurn - 0.5} " +
          s"(${state.playerWhite.morale} - ${state.playerBlack.morale}) " +
          s"${state.movesHistory.head.betterHumanString}")
    }

    movesHistory.reverse.scanLeft(startingState)((state, move) => state.playPlayerMove(move, turnUpdate = true)).foreach(show)
  }

  def play(startingState: GameState, playerWhiteStrategy: Strategy, playerBlackStrategy: Strategy): Unit = {
    val finalState = playFullGame(startingState, playerWhiteStrategy, playerBlackStrategy)
    showStateWithMoves(finalState)
    showAllGameStates(startingState, finalState.movesHistory)
  }

  def playFullGame(startingState: GameState, playerWhiteStrategy: Strategy, playerBlackStrategy: Strategy): GameState = {
    //    println(startingState)
    //    println(startingState.movesHistory.mkString("\n"))
    val strategy = startingState.getCurrentPlayer.team.chooseWhiteBlack(playerWhiteStrategy, playerBlackStrategy)
    strategy.chooseMove(startingState) match {
      case None =>
        showStateWithMoves(startingState)
        ???
      case Some(stateAfter) =>
        if (DEBUG_SHOW_TURNS) print(stateAfter.currentTurn + " ")
        if (stateAfter.winner != PlayerWinType.NotFinished) {
          if (DEBUG_SHOW_TURNS) println()
          stateAfter
        } else
          playFullGame(stateAfter, playerWhiteStrategy, playerBlackStrategy)
    }
  }

  def playAgainstExternalInput(startingState: GameState, strategy: Strategy): GameState = {
    def machineMove(state: GameState, history: List[GameState]): GameState = {
      val moveChosen = {
        val time = System.currentTimeMillis()
        val move = strategy.chooseMove(state)
        if (DEBUG_SHOW_CALC_TIME) {
          println(s"Turn calc time: ${System.currentTimeMillis() - time}")
        }
        move
      }
      moveChosen match {
        case None =>
          showStateWithMoves(state)
          ???
        case Some(stateAfter) =>
          println(stateAfter)
          println("Move made: " + stateAfter.movesHistory.head + "\n")
          if (stateAfter.winner != PlayerWinType.NotFinished) {
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
      if (index >= 26) {
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
      val input = scala.io.StdIn.readLine().toUpperCase
      if (input == "UNDO")
        inputIndex = Int.MinValue
      else
        inputIndex = Try(input.toInt).getOrElse(-1)
    } while (inputIndex > Int.MinValue && inputIndex < 0 || inputIndex >= allMoves.length)
    if (inputIndex == Int.MinValue)
      None
    else
      Some(gameState.playPlayerMove(allMoves(inputIndex), turnUpdate = true))
  }

}
