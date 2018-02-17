package ceo.play

import ceo.menu.MenuControl
import ceo.play.Util.{ValueOfStateMaxValue, random}

import scala.collection.mutable
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

      val values = startingState.generateAllNextStates.map(state =>
        (createTree(state, movesToLookAhead - 1, maximize = true, Int.MinValue, Int.MaxValue), state))

      val (value, endState) = values.maxBy(_._1)
      println((value, endState.movesHistory.head.betterHumanString))
      Some(endState)
    }
  }

  class ABPruningIterativeDeepening(timeLimit: Int, maxDepthToCheck: Int = 50) extends Strategy {
    override def chooseMove(_startingState: GameState): Option[GameState] = {
      val startingState =
        if (MenuControl.DEBUG_MODE) {
          _startingState
        } else {
          _startingState
        }
      val currentPlayer = startingState.getCurrentPlayer
      val currentPlayerColor = startingState.getCurrentPlayer.team
      val startingStateBaseValue = valueOfState(startingState, currentPlayerColor)

      val playerMoves = Util.random.shuffle(startingState.getCurrentPlayerMoves).sorted
      val firstLevelStates = playerMoves.map(move => startingState.playPlayerMove(move, turnUpdate = true))
      val firstLevelStatesSize = firstLevelStates.size.toDouble
      val showAtIndex = Math.max(1, firstLevelStatesSize / 5).toInt

      val startTime = System.currentTimeMillis()

      def shouldContinueCalculating: Boolean = MenuControl.inPause

      def createTreeFirstLevel(state: GameState, depth: Int, _alpha: Int, _beta: Int): Option[(Int, Int)] = {
        var alpha = _alpha
        var v = Int.MinValue
        var bestIndex = -1
        for (index <- firstLevelStates.indices) {
          if (System.currentTimeMillis - startTime < timeLimit || shouldContinueCalculating) {
            val state = firstLevelStates(index)
            if (index < 3 || index % showAtIndex == 0)
              if (MenuControl.SHOW_PRINTS)
                printf(index + " ")
            val value = createTree(state, depth - 1, maximize = false, alpha, _beta)
            if (value > v) {
              v = value
              bestIndex = index
            }
            alpha = Math.max(alpha, v)
            if (_beta <= alpha)
              return Some((v, bestIndex))
          } else {
            return None
          }
        }
        Some((v, bestIndex))
      }

      def createTree(state: GameState, depth: Int, maximize: Boolean, _alpha: Int, _beta: Int): Int = {
        if (depth == 0 || state.winner != PlayerWinType.NotFinished) {
          val value = valueOfState(state, currentPlayerColor)
          if (value >= ValueOfStateMaxValue / 2)
            value + depth * 1000 + state.getPlayer(currentPlayer.team).morale
          else if (value <= -ValueOfStateMaxValue / 3)
            value - depth * 1000 + state.getPlayer(currentPlayer.team).morale
          else
            value
        } else {
          val playerMoves = if (depth > 1) state.getCurrentPlayerMoves.sorted else state.getCurrentPlayerMoves
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

      if (MenuControl.SHOW_PRINTS)
        println(s"Calculating best move [${firstLevelStatesSize.toInt}]:")

      var value: Int = -1
      var moveIndex: Int = -1
      var stop = false

      val softTimeLimit: Int = (timeLimit * 0.3).toInt
      try {
        var currentDepth = 1
        while ((System.currentTimeMillis - startTime < softTimeLimit && currentDepth <= maxDepthToCheck || shouldContinueCalculating) && !stop) {
          if (MenuControl.SHOW_PRINTS)
            print(s"$currentDepth: ")
          createTreeFirstLevel(startingState, currentDepth, Int.MinValue, Int.MaxValue).foreach {
            case (levelBestValue, bestIndex) =>
              value = levelBestValue
              if (value >= ValueOfStateMaxValue || value <= -ValueOfStateMaxValue / 3)
                stop = true
              moveIndex = bestIndex
              if (MenuControl.SHOW_PRINTS)
                println(" %5d %5d %s".format(
                  System.currentTimeMillis - startTime,
                  value - startingStateBaseValue,
                  firstLevelStates(moveIndex).movesHistory.head)
                )
              currentDepth += 1
          }
        }
      } catch {
        case _: InterruptedException =>
          println("Finished calculating moves!")
      }

      val endState = firstLevelStates(moveIndex)
      if (MenuControl.SHOW_PRINTS) {
        println
        println((value, endState.movesHistory.head.betterHumanString))
      }

      var waitingCounter = 0
      while (shouldContinueCalculating) {
        if (waitingCounter % 10 == 0)
          println("Already finished calculating move... waiting for resume...")
        waitingCounter += 1
        Thread.sleep(1000 + waitingCounter * 10)
      }

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
              player.numberOfPieces * 20 +
              (if (player.hasKing) 0 else -(1000 + Math.max(0, 60 - player.morale) * 50)) + {
              // poison heuristic:
              player.piecesAffected.map(piece => piece.effectStatus.collectFirst {
                case EffectStatus.Poison(turnOfDeath) =>
                  val turnsLeft = gameState.currentTurn - turnOfDeath
                  // TODO this approach doesn't work with the king piece because it usually has 0 morale ...
                  if (turnsLeft >= 1.5)
                    -40 * piece.currentMorale
                  else
                    -80 * piece.currentMorale
              }.getOrElse(0)).sum + {
                val dir = player.directionForward.rowDiff
                var value = 0
                allPieces.foreach { piece =>
                  val data = piece.data
                  if (data.canMinionPromote)
                    value += piece.pos.row * dir
                  else if (data.isKing) {
                    if (player.numberOfPieces > 5) {
                      val row = piece.pos.row
                      if (player.team.isBottom) {
                        if (row < 6)
                          value += (6 - row) * -30
                      } else {
                        if (row > 1)
                          value += (row - 1) * -30
                      }
                    }
                    if (data.simpleName.endsWith("s"))
                      value -= 500
                  } else if (data.isDummyPiece)
                    value -= 10
                  if (data.isABlockerPiece && !piece.effectStatus.exists(_.effectType == EffectType.BlockAttacks))
                    value -= piece.currentMorale * 20
                }
                value
              }
            }
          }

          val whitePoints = value(gameState.playerWhite)
          val blackPoints = value(gameState.playerBlack)

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
