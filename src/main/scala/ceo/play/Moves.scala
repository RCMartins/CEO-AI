package ceo.play

sealed trait Moves {
  def getValidMove(piece: Piece, state: GameState, currentPlayer: Player): Option[PlayerMove]
}


object Moves {

  @inline private final def canRangedReachEnemy(piece: Piece, target: BoardPos, state: GameState): Option[Piece] = {
    if (piece.pos.allPosUntilAreEmptyOrGhost(target, state.board))
      target.getPiece(state.board).filter(_.team != piece.team)
    else
      None
  }

  private def canMove(piece: Piece, target: BoardPos, state: GameState): Option[PlayerMove.Move] = {
    if (piece.pos.allPosUntilAreEmptyOrGhost(target, state.board) && target.isEmpty(state.board))
      Some(PlayerMove.Move(piece, target))
    else
      None
  }

  private def canMoveUnblockable(piece: Piece, target: BoardPos, state: GameState): Option[PlayerMove] = {
    if (target.isEmpty(state.board))
      Some(PlayerMove.Move(piece, target))
    else
      None
  }

  private def canMoveFromStart(piece: Piece, target: BoardPos, state: GameState): Option[PlayerMove] = {
    if (piece.pos == piece.startingPosition)
      canMove(piece, target, state)
    else
      None
  }

  private def canAttack(piece: Piece, target: BoardPos, state: GameState): Option[PlayerMove] = {
    canRangedReachEnemy(piece, target, state) match {
      case Some(targetPiece) =>
        Some(PlayerMove.Attack(piece, targetPiece))
      case _ =>
        None
    }
  }

  private def canAttackUnblockable(piece: Piece, target: BoardPos, state: GameState): Option[PlayerMove] = {
    target.getPiece(state.board) match {
      case Some(targetPiece) if targetPiece.team != piece.team =>
        Some(PlayerMove.Attack(piece, targetPiece))
      case _ =>
        None
    }
  }

  private def canAttackUnblockableConditional(
    piece: Piece,
    target: BoardPos,
    state: GameState,
    condition: Piece => Boolean
  ): Option[PlayerMove] = {
    target.getPiece(state.board) match {
      case Some(targetPiece) if targetPiece.team != piece.team && condition(targetPiece) =>
        Some(PlayerMove.Attack(piece, targetPiece))
      case _ =>
        None
    }
  }

  private def canSwap(piece: Piece, target: BoardPos, state: GameState): Option[PlayerMove] = {
    target.getPiece(state.board) match {
      case Some(targetPiece) if targetPiece.team == piece.team =>
        Some(PlayerMove.Swap(piece, targetPiece))
      case _ =>
        None
    }
  }

  private def canRangedDestroy(piece: Piece, target: BoardPos, state: GameState): Option[PlayerMove] = {
    canRangedReachEnemy(piece, target, state) match {
      case Some(targetPiece) =>
        Some(PlayerMove.RangedDestroy(piece, targetPiece))
      case _ =>
        None
    }
  }

  private def canRangedPetrify(
    piece: Piece,
    target: BoardPos,
    durationTurns: Int,
    state: GameState
  ): Option[PlayerMove] = {
    canRangedReachEnemy(piece, target, state) match {
      case Some(targetPiece) =>
        Some(PlayerMove.RangedPetrify(piece, targetPiece, durationTurns))
      case _ =>
        None
    }
  }

  private def canMagicDestroy(piece: Piece, target: BoardPos, state: GameState): Option[PlayerMove] = {
    target.getPiece(state.board) match {
      case Some(targetPiece) if targetPiece.team != piece.team =>
        Some(PlayerMove.MagicDestroy(piece, targetPiece))
      case _ =>
        None
    }
  }

  private def canMagicPoison(
    piece: Piece,
    target: BoardPos,
    durationTurns: Int,
    state: GameState
  ): Option[PlayerMove] = {
    target.getPiece(state.board) match {
      case Some(targetPiece) if targetPiece.team != piece.team && !targetPiece.isPoisoned =>
        Some(PlayerMove.MagicPoison(piece, targetPiece, durationTurns))
      case _ =>
        None
    }
  }

  /**
    * TODO: this will not work if there are ghost piece before/after the path...
    */
  private def canTaurusRush(piece: Piece,
    target: BoardPos,
    maxDistance: Int,
    state: GameState
  ): Option[PlayerMove] = {
    def canMoveAndSeeEnemy(piece: Piece, target: BoardPos, state: GameState): Option[Piece] = {
      if (piece.pos.allPosUntilAreEmpty(target, state.board))
        target.getPiece(state.board).filter(_.team != piece.team)
      else
        None
    }

    canMoveAndSeeEnemy(piece, target, state) match {
      case Some(targetPiece) =>
        Some(PlayerMove.TaurusRush(piece, targetPiece, maxDistance))
      case _ =>
        None
    }
  }

  private def canTransformEnemyIntoAllyPiece(
    piece: Piece,
    target: BoardPos,
    moraleCost: Int,
    AllyPieceData: PieceData,
    state: GameState
  ): Option[PlayerMove] = {
    target.getPiece(state.board) match {
      case Some(targetPiece) if targetPiece.team != piece.team =>
        Some(PlayerMove.TransformEnemyIntoAllyPiece(piece, targetPiece, moraleCost, AllyPieceData))
      case _ =>
        None
    }
  }

  private def allyAt(target: BoardPos, state: GameState, currentPlayer: Player): Option[Piece] = {
    target.getPiece(state.board).filter(_.team == currentPlayer.team)
  }

  @inline private final def Or(first: Option[PlayerMove], second: => Option[PlayerMove]): Option[PlayerMove] = {
    first match {
      case None => second
      case _ => first
    }
  }

  case class MoveOrAttack(dist: Distance) extends Moves {
    def getValidMove(piece: Piece, state: GameState, currentPlayer: Player): Option[PlayerMove] = {
      Or(
        canMove(piece, piece.pos + dist, state),
        canAttack(piece, piece.pos + dist, state)
      )
    }
  }

  case class MoveOrAttackUnblockable(dist: Distance) extends Moves {
    def getValidMove(piece: Piece, state: GameState, currentPlayer: Player): Option[PlayerMove] = {
      Or(
        canMoveUnblockable(piece, piece.pos + dist, state),
        canAttackUnblockable(piece, piece.pos + dist, state)
      )
    }
  }

  case class MoveOrAttackOrSwapAlly(dist: Distance) extends Moves {
    def getValidMove(piece: Piece, state: GameState, currentPlayer: Player): Option[PlayerMove] = {
      Or(
        canMove(piece, piece.pos + dist, state),
        Or(canAttack(piece, piece.pos + dist, state),
          canSwap(piece, piece.pos + dist, state)
        ))
    }
  }

  //  case class MoveOrAttackOrBlock1Attack(dist: Distance) extends Moves

  case class Move(dist: Distance) extends Moves {
    def getValidMove(piece: Piece, state: GameState, currentPlayer: Player): Option[PlayerMove] = {
      canMove(piece, piece.pos + dist, state)
    }
  }

  case class MoveFromStart(dist: Distance) extends Moves {
    def getValidMove(piece: Piece, state: GameState, currentPlayer: Player): Option[PlayerMove] = {
      canMoveFromStart(piece, piece.pos + dist, state)
    }
  }

  case class MoveUnblockable(dist: Distance) extends Moves {
    def getValidMove(piece: Piece, state: GameState, currentPlayer: Player): Option[PlayerMove] = {
      canMoveUnblockable(piece, piece.pos + dist, state)
    }
  }

  case class Attack(dist: Distance) extends Moves {
    def getValidMove(piece: Piece, state: GameState, currentPlayer: Player): Option[PlayerMove] = {
      canAttack(piece, piece.pos + dist, state)
    }
  }

  case class RangedDestroy(dist: Distance) extends Moves {
    def getValidMove(piece: Piece, state: GameState, currentPlayer: Player): Option[PlayerMove] = {
      canRangedDestroy(piece, piece.pos + dist, state)
    }
  }

  case class MagicDestroy(dist: Distance) extends Moves {
    def getValidMove(piece: Piece, state: GameState, currentPlayer: Player): Option[PlayerMove] = {
      canMagicDestroy(piece, piece.pos + dist, state)
    }
  }

  case class RangedPetrify(dist: Distance, durationTurns: Int) extends Moves {
    def getValidMove(piece: Piece, state: GameState, currentPlayer: Player): Option[PlayerMove] = {
      canRangedPetrify(piece, piece.pos + dist, durationTurns, state)
    }
  }

  case class MagicPoison(dist: Distance, durationTurns: Int) extends Moves {
    def getValidMove(piece: Piece, state: GameState, currentPlayer: Player): Option[PlayerMove] = {
      canMagicPoison(piece, piece.pos + dist, durationTurns, state)
    }
  }

  case class TaurusRush(dist: Distance, maxDistance: Int) extends Moves {
    def getValidMove(piece: Piece, state: GameState, currentPlayer: Player): Option[PlayerMove] = {
      canTaurusRush(piece, piece.pos + dist, maxDistance, state)
    }
  }

  case class TransformEnemyIntoAllyPiece(dist: Distance, moraleCost: Int, allyPieceName: String) extends Moves {
    def getValidMove(piece: Piece, state: GameState, currentPlayer: Player): Option[PlayerMove] = {
      val allyUnitPieceData = DataLoader.getPieceData(allyPieceName, piece.data.team)
      canTransformEnemyIntoAllyPiece(piece, piece.pos + dist, moraleCost, allyUnitPieceData, state)
    }
  }

  case class Castling(posAllyPiece: Distance, posAfterKing: Distance, posAfterAllyPiece: Distance) extends Moves {
    def getValidMove(piece: Piece, state: GameState, currentPlayer: Player): Option[PlayerMove] = {
      // TODO check for ghost pieces in 'LONG' castling
      (canMove(piece, piece.pos + posAfterKing, state), allyAt(piece.pos + posAllyPiece, state, currentPlayer)) match {
        case (Some(kingMove), Some(allyPiece)) if !allyPiece.data.isMinion =>
          Some(PlayerMove.KingDoesCastling(piece, allyPiece, kingMove.to, piece.pos + posAfterAllyPiece))
        case _ =>
          None
      }
    }
  }

  case class JumpMinion(dist: Distance) extends Moves {
    def getValidMove(piece: Piece, state: GameState, currentPlayer: Player): Option[PlayerMove] = {
      canAttackUnblockableConditional(piece, piece.pos + dist, state, _.data.isMinion)
    }
  }

  case object Empty extends Moves {
    val dx = 0
    val dy = 0

    def getValidMove(piece: Piece, state: GameState, currentPlayer: Player): Option[PlayerMove] = ???
  }

  case object DummyMove extends Moves {
    val dx = 0
    val dy = 0

    def getValidMove(piece: Piece, state: GameState, currentPlayer: Player): Option[PlayerMove] = {
      Some(PlayerMove.DummyMove(piece))
    }
  }

}
