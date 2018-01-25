package ceo.play

import ceo.play.Util.random
import ceo.play.Util.ValueOfStateMaxValue
import ceo.play.PlayerTeam._

import scala.collection.parallel.immutable.ParSeq

sealed trait Strategy {
  def chooseMove(gameState: GameState): Option[GameState]

  def valueOfState(gameState: GameState, team: PlayerTeam): Int = gameState.defaultValueOfState(team)
}

object Strategy {

  trait oneMoveStrategy extends Strategy {
    override def chooseMove(startingState: GameState): Option[GameState] = {
      val currentPlayer = startingState.getCurrentPlayer
      val moves = startingState.getCurrentPlayerMoves
      val nextStates =
        moves.map(move => startingState.playPlayerMove(move, turnUpdate = true))

      val statesSorted =
        nextStates
          .map(after => (after, valueOfState(after, currentPlayer.team)))
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

    case class Node(state: GameState, nextStates: List[(Int, PlayerMove)], value: Int)

    override def chooseMove(startingState: GameState): Option[GameState] = {
      val currentPlayer = startingState.getCurrentPlayer.team

      def createTree(state: GameState, depth: Int, maximize: Boolean): Node = {
        if (depth == 0 || state.winner != PlayerWinType.NotFinished) {
          val value = valueOfState(state, currentPlayer)
          if (value == ValueOfStateMaxValue)
            Node(state, Nil, value + depth)
          else if (value == -ValueOfStateMaxValue || value == -ValueOfStateMaxValue / 2)
            Node(state, Nil, value - depth)
          else
            Node(state, Nil, value)
        } else {
          val playerMoves = state.getCurrentPlayerMoves
          val states = playerMoves.map(move => state.playPlayerMove(move, turnUpdate = true))

          val subTrees = states
            .map(state => createTree(state, depth - 1, !maximize))

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
      val endState = startingState.playPlayerMove(endMove, turnUpdate = true)
      Some(endState)
    }
  }

  case class MinMaxStrategyPar(movesToLookAhead: Int) extends Strategy {

    case class Node(state: GameState, nextStates: ParSeq[(Int, PlayerMove)], value: Int)

    override def chooseMove(startingState: GameState): Option[GameState] = {
      val currentPlayer = startingState.getCurrentPlayer.team

      def createTree(state: GameState, depth: Int, maximize: Boolean): Node = {
        if (depth == 0 || state.winner != PlayerWinType.NotFinished) {
          val value = valueOfState(state, currentPlayer)
          if (value == ValueOfStateMaxValue)
            Node(state, ParSeq.empty, value + depth)
          else if (value == -ValueOfStateMaxValue || value == -ValueOfStateMaxValue / 2)
            Node(state, ParSeq.empty, value - depth)
          else
            Node(state, ParSeq.empty, value)
        } else {
          val playerMoves = state.getCurrentPlayerMoves.par
          val states = playerMoves.map(move => state.playPlayerMove(move, turnUpdate = true))

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
      val endState = startingState.playPlayerMove(endMove, turnUpdate = true)
      Some(endState)
    }
  }

  case class AlphaBetaPruning(movesToLookAhead: Int) extends Strategy {
    override def chooseMove(startingState: GameState): Option[GameState] = {
      val currentPlayer = startingState.getCurrentPlayer.team

      def createTree(state: GameState, depth: Int, maximize: Boolean, _alpha: Int, _beta: Int): (Int, Int) = {
        if (depth == 0 || state.winner != PlayerWinType.NotFinished) {
          val value = valueOfState(state, currentPlayer)
          if (value == ValueOfStateMaxValue)
            (value + depth, -1)
          else if (value == -ValueOfStateMaxValue || value == -ValueOfStateMaxValue / 2)
            (value - depth, -1)
          else
            (value, -1)
        } else {
          val playerMoves = state.getCurrentPlayerMoves //Util.random.shuffle(state.getCurrentPlayerMoves)
          val states = playerMoves.map(move => state.playPlayerMove(move, turnUpdate = true))
          //          if (states.isEmpty)
          //            println(state)

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
      val states = playerMoves.map(move => startingState.playPlayerMove(move, turnUpdate = true))

      val endState = states(moveIndex)
      println((value, endState.movesHistory.head.betterHumanString))
      Some(endState)
    }
  }

  class AlphaBetaPruningIterativeDeepening(timeLimit: Int) extends Strategy {
    override def chooseMove(startingState: GameState): Option[GameState] = {
      val currentPlayer = startingState.getCurrentPlayer.team

      val playerMoves = startingState.getCurrentPlayerMoves
      val firstLevelStates = playerMoves.map(move => startingState.playPlayerMove(move, turnUpdate = true))
      val firstLevelStatesSize = playerMoves.size.toDouble
      val showAtIndex = if (firstLevelStatesSize <= 20) 1 else 2

      val startTime = System.currentTimeMillis()

      def createTreeFirstLevel(state: GameState, depth: Int, _alpha: Int, _beta: Int): Option[(Int, Array[Int])] = {
        def calcFinalValue(): Option[(Int, Array[Int])] = {
          var alpha = _alpha
          var v = Int.MinValue
          val moveValues = Array.fill(playerMoves.size)(-1)
          for (index <- firstLevelStates.indices) {
            if (System.currentTimeMillis - startTime < timeLimit) {
              val state = firstLevelStates(index)
              if (index % showAtIndex == 0)
                printf("%.2f ", index / firstLevelStatesSize)
              val value = createTree(state, depth - 1, maximize = false, alpha, _beta)
              if (value > v) {
                v = value
                moveValues(index) = v
              }
              alpha = Math.max(alpha, v)
              if (_beta <= alpha)
                return Some((v, moveValues))
            } else {
              return None
            }
          }
          Some((v, moveValues))
        }

        calcFinalValue()
      }

      def createTree(state: GameState, depth: Int, maximize: Boolean, _alpha: Int, _beta: Int): Int = {
        if (depth == 0 || state.winner != PlayerWinType.NotFinished) {
          val value = valueOfState(state, currentPlayer)
          if (value == ValueOfStateMaxValue)
            value + depth
          else if (value == -ValueOfStateMaxValue || value == -ValueOfStateMaxValue / 2)
            value - depth
          else
            value
        } else {
          val playerMoves = state.getCurrentPlayerMoves
          val states = playerMoves.map(move => state.playPlayerMove(move, turnUpdate = true))

          def calcFinalValue(): Int =
            if (maximize) {
              var alpha = _alpha
              var v = Int.MinValue
              states.foreach { state =>
                val value = createTree(state, depth - 1, !maximize, alpha, _beta)
                if (value > v)
                  v = value
                alpha = Math.max(alpha, v)
                if (_beta <= alpha)
                  return v
              }
              v
            } else {
              var beta = _beta
              var v = Int.MaxValue
              states.foreach { state =>
                val value = createTree(state, depth - 1, !maximize, _alpha, beta)
                if (value < v)
                  v = value
                beta = Math.min(beta, v)
                if (beta <= _alpha)
                  return v
              }
              v
            }

          calcFinalValue()
        }
      }

      println("Calculating best move:")

      var value: Int = -1
      var moveIndex: Int = -1
      var stop = false

      val softTimeLimit: Int = (timeLimit * 0.4).toInt
      try {
        var currentDepth = 1
        while (System.currentTimeMillis - startTime < softTimeLimit && !stop) {
          print(s"Calculating depth $currentDepth: ")
          createTreeFirstLevel(startingState, currentDepth, Int.MinValue, Int.MaxValue).foreach {
            case (levelBestValue, moveValues) =>
              value = levelBestValue
              if (value >= ValueOfStateMaxValue || value <= -ValueOfStateMaxValue)
                stop = true
              moveIndex = {
                val bestMoves = moveValues.zipWithIndex.filter(_._1 == value)
                bestMoves(random.nextInt(bestMoves.length))._2
              }
              println(" %6d   %6d   %s".format(System.currentTimeMillis - startTime, value, firstLevelStates(moveIndex).movesHistory.head))
              currentDepth += 1
          }
        }
      } catch {
        case _: InterruptedException =>
          println("Finished calculating moves!")
      }

      val endState = firstLevelStates(moveIndex)
      println((value, endState.movesHistory.head.betterHumanString))
      Some(endState)
    }
  }

}

object ValueOfState {

  trait ImprovedHeuristic extends Strategy {
    override def valueOfState(gameState: GameState, team: PlayerTeam): Int = {
      gameState.winner match {
        case PlayerWinType.NotFinished =>
          def value(player: Player): Int = {
            val allPieces = player.allPieces
            player.morale * 100 +
              player.numberOfPieces * 10 +
              (if (player.hasKing) 0 else -1000) + {
              // poison heuristic:
              player.piecesAffected.map(piece => piece.effectStatus.collectFirst {
                case EffectStatus.Poison(turnOfDeath) =>
                  val turnsLeft = gameState.currentTurn - turnOfDeath
                  if (turnsLeft >= 1.5)
                    -40 * piece.currentMorale
                  else if (turnsLeft >= 0.5)
                    -80 * piece.currentMorale
                  else
                    0
              }.getOrElse(0)).sum + {
                val dir = player.directionForward.rowDiff
                allPieces.map(piece => if (piece.data.canMinionPromote) piece.pos.row * dir else 0).sum
              } + {
                allPieces.count(_.data.isDummyPiece) * -10
              }
            }
          }

          val whitePoints = value(gameState.playerWhite)
          val blackPoints = value(gameState.playerBlack)

          //          team.chooseWhiteBlack(
          //            (1000 * whitePoints) / blackPoints,
          //            (1000 * blackPoints) / whitePoints
          //          )
          team.chooseWhiteBlack(
            whitePoints - blackPoints,
            blackPoints - whitePoints
          )
        case PlayerWinType.PlayerWhite => if (team.isWhite) ValueOfStateMaxValue else -ValueOfStateMaxValue
        case PlayerWinType.PlayerBlack => if (team.isBlack) ValueOfStateMaxValue else -ValueOfStateMaxValue
        case PlayerWinType.Draw => -ValueOfStateMaxValue / 2
      }
    }
  }

}
