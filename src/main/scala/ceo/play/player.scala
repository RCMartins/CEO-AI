package ceo.play

import ceo.play.Powers._
import com.softwaremill.quicklens._

case class Player(
  team: PlayerTeam,
  morale: Int,
  pieces: List[Piece] = List.empty,
  piecesAffected: List[Piece] = List.empty,
  numberOfPieces: Int,
  kingMode: Int,
  extraData: PlayerExtraData
) {

  override def toString: String = s"Player$team($morale)"

  def hasKing: Boolean = kingMode > 0

  def allPieces: List[Piece] = pieces ++ piecesAffected

  def changeMorale(diff: Int): Player = copy(morale = morale + diff)

  def removePiece(piece: Piece): Player = {
    def removeFirst(list: List[Piece]): List[Piece] = list match {
      case Nil => Nil
      case x :: xs => if (x eq piece) xs else x :: removeFirst(xs)
    }

    if (piece.data.isKing)
      copy(pieces = removeFirst(pieces),
        piecesAffected = removeFirst(piecesAffected), numberOfPieces = numberOfPieces - 1, kingMode = kingMode - 1)
    else
      copy(pieces = removeFirst(pieces),
        piecesAffected = removeFirst(piecesAffected), numberOfPieces = numberOfPieces - 1)
  }

  def placePiece(piece: Piece): Player = {
    val isAffected = piece.effectStatus.nonEmpty
    if (piece.data.isKing) {
      if (isAffected)
        copy(piecesAffected = piece :: piecesAffected, numberOfPieces = numberOfPieces + 1, kingMode = if (kingMode <= 0) 1 else kingMode + 1)
      else
        copy(pieces = piece :: pieces, numberOfPieces = numberOfPieces + 1, kingMode = if (kingMode <= 0) 1 else kingMode + 1)
    } else {
      if (isAffected)
        copy(piecesAffected = piece :: piecesAffected, numberOfPieces = numberOfPieces + 1)
      else
        copy(pieces = piece :: pieces, numberOfPieces = numberOfPieces + 1)
    }
  }

  // TODO: not used yet
  def killAt(boardPos: BoardPos): Player =
    copy(extraData = extraData.copy(fallenPiecesPositions = extraData.fallenPiecesPositions + boardPos))

  def updateGuardedPositions(toRemove: Option[Piece], toAdd: Option[Piece]): Player = {
    val updatedGuardedPositions =
      (toRemove, toAdd) match {
        case (Some(piece1), Some(piece2)) =>
          extraData.guardedPositions -- piece1.data.guardedPositions.map(piece1.pos + _) ++
            piece2.data.guardedPositions.map(dist => (piece2.pos + dist) -> piece2)
        case (Some(piece), None) =>
          extraData.guardedPositions -- piece.data.guardedPositions.map(piece.pos + _)
        case (None, Some(piece)) =>
          extraData.guardedPositions ++ piece.data.guardedPositions.map(dist => (piece.pos + dist) -> piece)
        case (None, None) =>
          extraData.guardedPositions
      }
    copy(extraData = extraData.copy(guardedPositions = updatedGuardedPositions))
  }

  def inBaseRow(pos: BoardPos): Boolean = pos.row == (if (team.isBottom) 7 else 0)

  def directionForward: Distance = if (team.isBottom) Distance(-1, 0) else Distance(1, 0)

  def optimizeRunners(gameState: GameState): Player = modify(this)(_.extraData).using(_.copy(
    globalDeathPieceRunners = createGlobalDeathPieceRunners(gameState),
    hasEndOfTurnTriggers = allPieces.exists(_.data.powers.exists { case _: MagicTriggerLust => true; case _ => false })
  ))

  def createGlobalDeathPieceRunners(gameState: GameState): List[DynamicRunner[GameState, Piece /* piece that died */ ]] = {
    /**
      * TODO: there is a problem here if we don't consider that enemy units can be charmed while checking several turns ahead ...
      * So, if any of that units are in the game, we should check if we have some of them
      * (OR really complex check if it is possible to charm those pieces (only to be more optimized - not to be correct))
      * (Same problem for lust "hasEndOfTurnTriggers")
      *
      * val all = gameState.allPieces ...
      */

    (allPieces.find(_.data.powers.contains(OnEnemyDeathMovesForward)) match {
      case None =>
        List.empty
      case Some(_) =>
        List(new DynamicRunner[GameState, Piece] {
          override def update(startingState: GameState, deadPiece: Piece): GameState = {
            if (deadPiece.team == team || !allPieces.exists(_.data.powers.contains(OnEnemyDeathMovesForward)))
              startingState
            else {
              startingState.addEndOfTurnAction(EndOfTurnAction.MoveDoves(team))
            }
          }
        })
    }) ++ (allPieces.find(_.data.powers.exists { case _: TriggerWrathOnAdjacentAllyDeath => true; case _ => false }) match {
      case None =>
        List.empty
      case Some(_) =>
        List(new DynamicRunner[GameState, Piece] {
          override def update(startingState: GameState, deadPiece: Piece): GameState = {
            if (deadPiece.team != team)
              startingState
            else {
              val enemyPlayer = startingState.getPlayer(team.enemy)
              if (!enemyPlayer.hasKing)
                startingState
              else {
                var wrathDurationOption = Option.empty[Int]
                val pos = deadPiece.pos
                Distance.adjacentDistances.map(pos + _).exists { adjacentPos =>
                  adjacentPos.getPiece(startingState.board).exists { piece =>
                    piece.team == team && piece.data.powers.collectFirst {
                      case Powers.TriggerWrathOnAdjacentAllyDeath(turnsToLightUpLocation) =>
                        wrathDurationOption = Some(turnsToLightUpLocation)
                      case _ =>
                    }.isDefined
                  }
                }
                wrathDurationOption match {
                  case None => startingState
                  case Some(wrathDuration) =>
                    val kingPiece = enemyPlayer.allPieces.find(_.data.isKing).get
                    val lightning = BoardEffect.Lightning(kingPiece.pos, startingState.currentTurn + wrathDuration)
                    startingState.copy(boardEffects = lightning :: startingState.boardEffects)
                }
              }
            }
          }
        })
    })
}

case class PlayerExtraData(
  fallenPiecesPositions: Set[BoardPos],
  fallenPieces: List[PieceData],
  guardedPositions: Map[BoardPos, Piece],
  globalDeathPieceRunners: List[DynamicRunner[GameState, Piece /* piece that died */ ]],
  hasEndOfTurnTriggers: Boolean
)

object PlayerExtraData {
  val empty: PlayerExtraData = PlayerExtraData(Set.empty, List.empty, Map.empty, List.empty, hasEndOfTurnTriggers = false)
}
