package ceo.play

import scala.util.Random

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
        Some(bestMoves(Random.nextInt(bestMoves.length))._1)
      }
    }
  }

  case class MinMaxStrategyBM(movesToLookAhead: Int) extends Strategy {
    override def chooseMove(startingState: GameState): Option[GameState] = {
      val currentPlayer = startingState.getCurrentPlayer.team

      case class Node(state: GameState, nextStates: List[Node], value: Int)

      def createTree(state: GameState, depth: Int, maximize: Boolean): Node = {
        //        println("#" * 70 + s" $depth " + "#" * 70 + s"\n$state")
        if (depth == 0 || state.winner.isDefined) {
          Node(state, Nil, state.valueOfState(currentPlayer))
        } else {
          val moves = state.getCurrentPlayerMoves
          val subTrees =
            moves
              .map(move => state.playPlayerMove(move))
              .map(state => createTree(state, depth - 1, !maximize))

          val finalValue = if (maximize) subTrees.maxBy(_.value).value else subTrees.minBy(_.value).value
          Node(state, subTrees, finalValue)
        }
      }

      val finalNode =
        createTree(startingState, movesToLookAhead, maximize = true)

      //      println(finalNode.nextStates.map(_.value))

      val finalStates = finalNode.nextStates.map(node => (node.value, node.state))
      val statesSorted = finalStates.sortBy(-_._1)
      val bestMoves =
        statesSorted.takeWhile(_._1 == statesSorted.head._1)

      //      println(bestMoves.mkString("\n\n\n"))
      Some(bestMoves(Random.nextInt(bestMoves.length))._2)
    }
  }

  case class MinMaxStrategy(movesToLookAhead: Int) extends Strategy {
    private val MaxValue = 1e9.toInt

    override def chooseMove(startingState: GameState): Option[GameState] = {
      val currentPlayer = startingState.getCurrentPlayer.team

      case class Node(state: GameState, nextStates: List[Node], value: Int)

      def createTree(state: GameState, depth: Int, maximize: Boolean): Node = {
        if (depth == 0 || state.winner.isDefined) {
          val value = state.valueOfState(currentPlayer)
          if (value == MaxValue)
            Node(state, Nil, value + depth)
          else
            Node(state, Nil, state.valueOfState(currentPlayer))
        } else {
          val moves = state.getCurrentPlayerMoves
          val subTrees = moves
            .map(move => createTree(state.playPlayerMove(move), depth - 1, !maximize))

          val finalValue = if (maximize) subTrees.maxBy(_.value).value else subTrees.minBy(_.value).value
          Node(state, subTrees, finalValue)
        }
      }

      val finalNode =
        createTree(startingState, movesToLookAhead, maximize = true)

      val finalStates = finalNode.nextStates.map(node => (node.value, node.state))
      val statesSorted = finalStates.sortBy(-_._1)
      val bestMoves =
        statesSorted.count(_._1 == statesSorted.head._1)

      Some(statesSorted(Random.nextInt(bestMoves))._2)
    }
  }

}
