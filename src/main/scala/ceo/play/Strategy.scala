package ceo.play

import Util.random

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

    case class Node(state: GameState, nextStates: List[(Int, GameState)], value: Int)

    //    var countNodes = 0
    //    var countBranches = 0

    override def chooseMove(startingState: GameState): Option[GameState] = {
      val currentPlayer = startingState.getCurrentPlayer.team

//      val levelsCount = Array.ofDim[Int](movesToLookAhead + 1)
//      var hit = 0

      def createTree(state: GameState, depth: Int, maximize: Boolean): Node = {
//        levelsCount(depth) += 1

        if (depth == 0 || state.winner.isDefined) {
          val value = state.valueOfState(currentPlayer)
          if (value == MaxValue)
            Node(state, Nil, value + depth)
          else if (value == -MaxValue)
            Node(state, Nil, value - depth)
          else
            Node(state, Nil, value)
        } else {
          val states = state.getCurrentPlayerMoves.map(state.playPlayerMove)
          if (states.isEmpty)
            println(state)
//          else if (depth == movesToLookAhead)
//            println("total: " + states.length)

          val subTrees = states
            .map(state => createTree(state, depth - 1, !maximize))

//          hit += 1
//          if (hit % 10000 == 0)
//            println(levelsCount.toList)

          val finalValue = if (maximize) subTrees.maxBy(_.value).value else subTrees.minBy(_.value).value
          Node(state, if (depth == movesToLookAhead) subTrees.map(node => (node.value, node.state)) else Nil, finalValue)
        }
      }

      val finalNode =
        createTree(startingState, movesToLookAhead, maximize = true)

      val finalStates = finalNode.nextStates
      val statesSorted = finalStates.sortBy(-_._1)
      val bestMoves =
        statesSorted.count(_._1 == statesSorted.head._1)

      println(statesSorted.map { case (value, state) => (value, state.movesHistory.head) }.mkString("\n"))
      Some(statesSorted(random.nextInt(bestMoves))._2)
    }
  }

  case class MinMaxStrategyPar(movesToLookAhead: Int) extends Strategy {
    private val MaxValue = 1e9.toInt

    case class Node(state: GameState, nextStates: ParSeq[Node], value: Int)

    override def chooseMove(startingState: GameState): Option[GameState] = {
      val currentPlayer = startingState.getCurrentPlayer.team

      def createTree(state: GameState, depth: Int, maximize: Boolean): Node = {
        if (depth == 0 || state.winner.isDefined) {
          val value = state.valueOfState(currentPlayer)
          if (value == MaxValue)
            Node(state, ParSeq.empty, value + depth)
          else if (value == -MaxValue)
            Node(state, ParSeq.empty, value - depth)
          else
            Node(state, ParSeq.empty, value)
        } else {
          val moves = state.getCurrentPlayerMoves.par
          val subTrees = moves
            .map(move => createTree(state.playPlayerMove(move), depth - 1, !maximize))

          val finalValue = if (maximize) subTrees.maxBy(_.value).value else subTrees.minBy(_.value).value
          Node(state, subTrees, finalValue)
        }
      }

      val finalNode =
        createTree(startingState, movesToLookAhead, maximize = true)

      val finalStates = finalNode.nextStates.map(node => (node.value, node.state)).toList
      val statesSorted = finalStates.sortBy(-_._1)
      val bestMoves =
        statesSorted.count(_._1 == statesSorted.head._1)

      Some(statesSorted(random.nextInt(bestMoves))._2)
    }
  }

}