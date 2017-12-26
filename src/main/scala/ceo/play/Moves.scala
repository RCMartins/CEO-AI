package ceo.play

sealed trait Moves {
  def getValidMove(piece: Piece, state: GameState, currentPlayer: Player): Option[PlayerMove]
}


object Moves {

  @inline private final def canMoveAndSeeEnemy(piece: Piece, target: BoardPos, state: GameState, currentPlayer: Player): Option[Piece] = {
    if (piece.pos.allPosUntilAreEmpty(target, state.board))
      target.getPiece(state.board).filter(_.team != currentPlayer.team)
    else
      None
  }

  private def canMove(piece: Piece, target: BoardPos, state: GameState, currentPlayer: Player): Option[PlayerMove.Move] = {
    if (piece.pos.allPosToAreEmpty(target, state.board))
      Some(PlayerMove.Move(piece, target))
    else
      None
  }

  private def canMoveUnblockable(piece: Piece, target: BoardPos, state: GameState, currentPlayer: Player): Option[PlayerMove] = {
    if (target.isEmpty(state.board))
      Some(PlayerMove.Move(piece, target))
    else
      None
  }

  private def canMoveFromStart(piece: Piece, target: BoardPos, state: GameState, currentPlayer: Player): Option[PlayerMove] = {
    if (!piece.hasMoved)
      canMove(piece, target, state, currentPlayer)
    else
      None
  }

  private def canAttack(piece: Piece, target: BoardPos, state: GameState, currentPlayer: Player): Option[PlayerMove] = {
    canMoveAndSeeEnemy(piece, target, state, currentPlayer) match {
      case Some(targetPiece) =>
        Some(PlayerMove.Attack(piece, targetPiece))
      case _ =>
        None
    }
  }

  private def canAttackUnblockable(piece: Piece, target: BoardPos, state: GameState, currentPlayer: Player): Option[PlayerMove] = {
    target.getPiece(state.board) match {
      case Some(targetPiece) if targetPiece.team != currentPlayer.team =>
        Some(PlayerMove.Attack(piece, targetPiece))
      case _ =>
        None
    }
  }

  private def canAttackUnblockableConditional(
    piece: Piece,
    target: BoardPos,
    state: GameState,
    currentPlayer: Player,
    condition: Piece => Boolean
  ): Option[PlayerMove] = {
    target.getPiece(state.board) match {
      case Some(targetPiece) if targetPiece.team != currentPlayer.team && condition(targetPiece) =>
        Some(PlayerMove.Attack(piece, targetPiece))
      case _ =>
        None
    }
  }

  private def canSwap(piece: Piece, target: BoardPos, state: GameState, currentPlayer: Player): Option[PlayerMove] = {
    target.getPiece(state.board) match {
      case Some(targetPiece) if targetPiece.team == currentPlayer.team =>
        Some(PlayerMove.Swap(piece, targetPiece))
      case _ =>
        None
    }
  }

  private def canRangedDestroy(piece: Piece, target: BoardPos, state: GameState, currentPlayer: Player): Option[PlayerMove] = {
    canMoveAndSeeEnemy(piece, target, state, currentPlayer) match {
      case Some(targetPiece) =>
        Some(PlayerMove.RangedDestroy(piece, targetPiece))
      case _ =>
        None
    }
  }

  private def canMagicDestroy(piece: Piece, target: BoardPos, state: GameState, currentPlayer: Player): Option[PlayerMove] = {
    target.getPiece(state.board) match {
      case Some(targetPiece) if targetPiece.team != currentPlayer.team =>
        Some(PlayerMove.MagicDestroy(piece, targetPiece))
      case _ =>
        None
    }
  }

  private def canRangedPetrify(
    piece: Piece,
    target: BoardPos,
    durationTurns: Int,
    state: GameState,
    currentPlayer: Player
  ): Option[PlayerMove] = {
    canMoveAndSeeEnemy(piece, target, state, currentPlayer) match {
      case Some(targetPiece) =>
        Some(PlayerMove.RangedPetrify(piece, targetPiece, durationTurns))
      case _ =>
        None
    }
  }

  private def canMagicPoison(
    piece: Piece,
    target: BoardPos,
    durationTurns: Int,
    state: GameState,
    currentPlayer: Player
  ): Option[PlayerMove] = {
    target.getPiece(state.board) match {
      case Some(targetPiece) if targetPiece.team != currentPlayer.team && !targetPiece.isPoisoned =>
        Some(PlayerMove.MagicPoison(piece, targetPiece, durationTurns))
      case _ =>
        None
    }
  }

  private def canTaurusRush(piece: Piece,
    target: BoardPos,
    maxDistance: Int,
    state: GameState,
    currentPlayer: Player
  ): Option[PlayerMove] = {
    canMoveAndSeeEnemy(piece, target, state, currentPlayer) match {
      case Some(targetPiece) =>
        Some(PlayerMove.TaurusRush(piece, targetPiece, maxDistance))
      case _ =>
        None
    }
  }

  private def canTransformEnemyIntoAllyUnit(
    piece: Piece,
    target: BoardPos,
    moraleCost: Int,
    AllyPieceData: PieceData,
    state: GameState,
    currentPlayer: Player
  ): Option[PlayerMove] = {
    target.getPiece(state.board) match {
      case Some(targetPiece) if targetPiece.team == currentPlayer.team.enemy =>
        Some(PlayerMove.TransformEnemyIntoAllyUnit(piece, targetPiece, moraleCost, AllyPieceData))
      case _ =>
        None
    }
  }

  private def allyAt(target: BoardPos, state: GameState, currentPlayer: Player): Option[Piece] = {
    target.getPiece(state.board) collect {
      case targetPiece if targetPiece.team == currentPlayer.team => targetPiece
    }
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
        canMove(piece, piece.pos + dist, state, currentPlayer),
        canAttack(piece, piece.pos + dist, state, currentPlayer)
      )
    }
  }

  case class MoveOrAttackUnblockable(dist: Distance) extends Moves {
    def getValidMove(piece: Piece, state: GameState, currentPlayer: Player): Option[PlayerMove] = {
      Or(
        canMoveUnblockable(piece, piece.pos + dist, state, currentPlayer),
        canAttackUnblockable(piece, piece.pos + dist, state, currentPlayer)
      )
    }
  }

  case class MoveOrAttackOrSwapAlly(dist: Distance) extends Moves {
    def getValidMove(piece: Piece, state: GameState, currentPlayer: Player): Option[PlayerMove] = {
      Or(
        canMove(piece, piece.pos + dist, state, currentPlayer),
        Or(canAttack(piece, piece.pos + dist, state, currentPlayer),
          canSwap(piece, piece.pos + dist, state, currentPlayer)
        ))
    }
  }

  //  case class MoveOrAttackOrBlock1Attack(dist: Distance) extends Moves

  case class Move(dist: Distance) extends Moves {
    def getValidMove(piece: Piece, state: GameState, currentPlayer: Player): Option[PlayerMove] = {
      canMove(piece, piece.pos + dist, state, currentPlayer)
    }
  }

  case class MoveFromStart(dist: Distance) extends Moves {
    def getValidMove(piece: Piece, state: GameState, currentPlayer: Player): Option[PlayerMove] = {
      canMoveFromStart(piece, piece.pos + dist, state, currentPlayer)
    }
  }

  case class MoveUnblockable(dist: Distance) extends Moves {
    def getValidMove(piece: Piece, state: GameState, currentPlayer: Player): Option[PlayerMove] = {
      canMoveUnblockable(piece, piece.pos + dist, state, currentPlayer)
    }
  }

  case class Attack(dist: Distance) extends Moves {
    def getValidMove(piece: Piece, state: GameState, currentPlayer: Player): Option[PlayerMove] = {
      canAttack(piece, piece.pos + dist, state, currentPlayer)
    }
  }

  case class RangedDestroy(dist: Distance) extends Moves {
    def getValidMove(piece: Piece, state: GameState, currentPlayer: Player): Option[PlayerMove] = {
      canRangedDestroy(piece, piece.pos + dist, state, currentPlayer)
    }
  }

  case class MagicDestroy(dist: Distance) extends Moves {
    def getValidMove(piece: Piece, state: GameState, currentPlayer: Player): Option[PlayerMove] = {
      canMagicDestroy(piece, piece.pos + dist, state, currentPlayer)
    }
  }

  case class RangedPetrify(dist: Distance, durationTurns: Int) extends Moves {
    def getValidMove(piece: Piece, state: GameState, currentPlayer: Player): Option[PlayerMove] = {
      canRangedPetrify(piece, piece.pos + dist, durationTurns, state, currentPlayer)
    }
  }

  case class MagicPoison(dist: Distance, durationTurns: Int) extends Moves {
    def getValidMove(piece: Piece, state: GameState, currentPlayer: Player): Option[PlayerMove] = {
      canMagicPoison(piece, piece.pos + dist, durationTurns, state, currentPlayer)
    }
  }

  case class TaurusRush(dist: Distance, maxDistance: Int) extends Moves {
    def getValidMove(piece: Piece, state: GameState, currentPlayer: Player): Option[PlayerMove] = {
      canTaurusRush(piece, piece.pos + dist, maxDistance, state, currentPlayer)
    }
  }

  case class TransformEnemyIntoAllyUnit(dist: Distance, moraleCost: Int, allyUnitName: String) extends Moves {
    def getValidMove(piece: Piece, state: GameState, currentPlayer: Player): Option[PlayerMove] = {
      val allyUnitPieceData = DataLoader.getPieceData(allyUnitName, piece.data.team)
      canTransformEnemyIntoAllyUnit(piece, piece.pos + dist, moraleCost, allyUnitPieceData, state, currentPlayer)
    }
  }

  case class Castling(posAllyPiece: Distance, posAfterKing: Distance, posAfterAllyPiece: Distance) extends Moves {
    def getValidMove(piece: Piece, state: GameState, currentPlayer: Player): Option[PlayerMove] = {
      (canMove(piece, piece.pos + posAfterKing, state, currentPlayer), allyAt(piece.pos + posAllyPiece, state, currentPlayer)) match {
        case (Some(kingMove), Some(allyPiece)) if !allyPiece.data.isMinion =>
          Some(PlayerMove.KingDoesCastling(piece, allyPiece, kingMove.to, piece.pos + posAfterAllyPiece))
        case _ =>
          None
      }
    }
  }

  case class JumpMinion(dist: Distance) extends Moves {
    def getValidMove(piece: Piece, state: GameState, currentPlayer: Player): Option[PlayerMove] = {
      canAttackUnblockableConditional(piece, piece.pos + dist, state, currentPlayer, _.data.isMinion)
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
