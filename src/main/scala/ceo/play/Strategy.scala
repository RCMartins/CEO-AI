package ceo.play

import ceo.play.Util.random

import scala.collection.parallel.immutable.ParSeq

trait Strategy {
  def chooseMove(gameState: GameState): Option[GameState]
}

object Strategy {

  object oneMoveStrategy extends Strategy {
    override def chooseMove(startingState: GameState): Option[GameState] = {
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
        Some(bestMoves(random.nextInt(bestMoves.length))._1)
      }
    }
  }

  case class MinMaxStrategy(movesToLookAhead: Int) extends Strategy {
    private val MaxValue = 1e9.toInt

    case class Node(state: GameState, nextStates: List[(Int, PlayerMove)], value: Int)

    //    var countNodes = 0
    //    var countBranches = 0

    override def chooseMove(startingState: GameState): Option[GameState] = {
      val currentPlayer = startingState.getCurrentPlayer.team

      //      val levelsCount = Array.ofDim[Int](movesToLookAhead + 1)
      //      var hit = 0

      def createTree(state: GameState, depth: Int, maximize: Boolean): Node = {
        //        levelsCount(depth) += 1

        if (depth == 0 || state.winner != PlayerWinType.NotFinished) {
          val value = state.valueOfState(currentPlayer)
          if (value == MaxValue)
            Node(state, Nil, value + depth)
          else if (value == -MaxValue || value == -MaxValue / 2)
            Node(state, Nil, value - depth)
          else
            Node(state, Nil, value)
        } else {
          val playerMoves = state.getCurrentPlayerMoves
          val states = playerMoves.map(state.playPlayerMove)
          if (states.isEmpty)
            println(state)
          //          else if (depth == movesToLookAhead)
          //            println("total: " + states.length)

          val subTrees = states
            .map(state => createTree(state, depth - 1, !maximize))

          //          hit += 1
          //          if (hit % 10000 == 0)
          //            println(levelsCount.toList)

          val finalValue = if (maximize) subTrees.view.map(_.value).max else subTrees.view.map(_.value).min
          val nextStates =
            if (depth == movesToLookAhead)
              subTrees.zip(playerMoves).map { case (node, move) => (node.value, move) }
            else
              Nil
          Node(state, nextStates, finalValue)
        }
      }

      val finalNode =
        createTree(startingState, movesToLookAhead, maximize = true)

      val finalStates = finalNode.nextStates
      val statesSorted = finalStates.sortBy(-_._1)
      val bestMoves =
        statesSorted.count(_._1 == statesSorted.head._1)

      println(statesSorted.mkString("\n"))
      val endMove = statesSorted(random.nextInt(bestMoves))._2
      val endState = startingState.playPlayerMove(endMove)
      Some(endState)
    }
  }

  case class MinMaxStrategyPar(movesToLookAhead: Int) extends Strategy {
    private val MaxValue = 1e9.toInt

    case class Node(state: GameState, nextStates: ParSeq[(Int, PlayerMove)], value: Int)

    override def chooseMove(startingState: GameState): Option[GameState] = {
      val currentPlayer = startingState.getCurrentPlayer.team

      def createTree(state: GameState, depth: Int, maximize: Boolean): Node = {
        if (depth == 0 || state.winner != PlayerWinType.NotFinished) {
          val value = state.valueOfState(currentPlayer)
          if (value == MaxValue)
            Node(state, ParSeq.empty, value + depth)
          else if (value == -MaxValue || value == -MaxValue / 2)
            Node(state, ParSeq.empty, value - depth)
          else
            Node(state, ParSeq.empty, value)
        } else {
          val playerMoves = state.getCurrentPlayerMoves.par
          val states = playerMoves.map(state.playPlayerMove)

          val subTrees = states
            .map(state => createTree(state, depth - 1, !maximize))

          val finalValue = if (maximize) subTrees.seq.view.map(_.value).max else subTrees.seq.view.map(_.value).min
          val nextStates =
            if (depth == movesToLookAhead)
              subTrees.zip(playerMoves).map { case (node, move) => (node.value, move) }
            else
              ParSeq.empty
          Node(state, nextStates, finalValue)
        }
      }

      val finalNode =
        createTree(startingState, movesToLookAhead, maximize = true)

      val finalStates = finalNode.nextStates.toList
      val statesSorted = finalStates.sortBy(-_._1)
      val bestMoves =
        statesSorted.count(_._1 == statesSorted.head._1)

      println(statesSorted.mkString("\n"))
      val endMove = statesSorted(random.nextInt(bestMoves))._2
      val endState = startingState.playPlayerMove(endMove)
      Some(endState)
    }
  }

  case class AlphaBetaPruning(movesToLookAhead: Int) extends Strategy {
    private val MaxValue = 1e9.toInt

    override def chooseMove(startingState: GameState): Option[GameState] = {
      val currentPlayer = startingState.getCurrentPlayer.team

      def createTree(state: GameState, depth: Int, maximize: Boolean, _alpha: Int, _beta: Int): (Int, Int) = {
        if (depth == 0 || state.winner != PlayerWinType.NotFinished) {
          val value = state.valueOfState(currentPlayer)
          if (value == MaxValue)
            (value + depth, -1)
          else if (value == -MaxValue || value == -MaxValue / 2)
            (value - depth, -1)
          else
            (value, -1)
        } else {
          val playerMoves = state.getCurrentPlayerMoves //Util.random.shuffle(state.getCurrentPlayerMoves)
          val states = playerMoves.map(state.playPlayerMove)
          if (states.isEmpty)
            println(state)

          def calcFinalValue(): (Int, Int) =
            if (maximize) {
              var alpha = _alpha
              var v = Int.MinValue
              var index = 0
              var bestIndex = -1
              states.foreach { state =>
                val value = createTree(state, depth - 1, !maximize, alpha, _beta)._1
                if (value > v) {
                  v = value
                  bestIndex = index
                }
                alpha = Math.max(alpha, v)
                if (_beta <= alpha)
                  return (v, -1)
                index += 1
              }
              (v, bestIndex)
            } else {
              var beta = _beta
              var v = Int.MaxValue
              var index = 0
              var bestIndex = -1
              states.foreach { state =>
                val value = createTree(state, depth - 1, !maximize, _alpha, beta)._1
                if (value < v) {
                  v = value
                  bestIndex = index
                }
                beta = Math.min(beta, v)
                if (beta <= _alpha)
                  return (v, index)
                index += 1
              }
              (v, bestIndex)
            }

          calcFinalValue()
        }
      }

      val (value, moveIndex) =
        createTree(startingState, movesToLookAhead, maximize = true, Int.MinValue, Int.MaxValue)

      val playerMoves = startingState.getCurrentPlayerMoves
      val states = playerMoves.map(startingState.playPlayerMove)

      val endState = states(moveIndex)
      println((value, endState.movesHistory.head.betterHumanString))
      Some(endState)
    }
  }

}
