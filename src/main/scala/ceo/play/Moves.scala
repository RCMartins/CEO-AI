package ceo.play

sealed trait Moves {
  def getValidMove(piece: Piece, state: GameState, currentPlayer: Player): Option[PlayerMove]

  def dx: Int

  def dy: Int
}

object Moves {

  @inline private def hasNoFreezeTypeEffect(piece: Piece): Boolean = {
    piece.effectStatus.forall {
      case _: EffectStatus.Petrified => false
      case _ => true
    }
  }

  @inline private def canMoveAndSeeEnemy(piece: Piece, target: BoardPos, state: GameState, currentPlayer: Player): Option[Piece] = {
    if (hasNoFreezeTypeEffect(piece) && piece.pos.allPosUntilAreEmpty(target, state.board))
      target.getPiece(state.board).filter(_.team == currentPlayer.team.enemy)
    else
      None
  }

  private def canMove(piece: Piece, target: BoardPos, state: GameState, currentPlayer: Player): Option[PlayerMove] = {
    if (hasNoFreezeTypeEffect(piece) && piece.pos.allPosToAreEmpty(target, state.board))
      Some(PlayerMove.Move(piece, target))
    else
      None
  }

  private def canMoveUnblockable(piece: Piece, target: BoardPos, state: GameState, currentPlayer: Player): Option[PlayerMove] = {
    if (hasNoFreezeTypeEffect(piece) && target.isEmpty(state.board))
      Some(PlayerMove.Move(piece, target))
    else
      None
  }

  private def canMoveFromStart(piece: Piece, target: BoardPos, state: GameState, currentPlayer: Player): Option[PlayerMove] = {
    if (hasNoFreezeTypeEffect(piece) && !piece.hasMoved)
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
    (hasNoFreezeTypeEffect(piece), target.getPiece(state.board)) match {
      case (true, Some(targetPiece)) if targetPiece.team == currentPlayer.team.enemy =>
        Some(PlayerMove.Attack(piece, targetPiece))
      case _ =>
        None
    }
  }

  private def canSwap(piece: Piece, target: BoardPos, state: GameState, currentPlayer: Player): Option[PlayerMove] = {
    (hasNoFreezeTypeEffect(piece), target.getPiece(state.board)) match {
      case (true, Some(targetPiece)) if targetPiece.team == currentPlayer.team =>
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
    (hasNoFreezeTypeEffect(piece), target.getPiece(state.board)) match {
      case (true, Some(targetPiece)) if targetPiece.team == currentPlayer.team.enemy =>
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
    (hasNoFreezeTypeEffect(piece), target.getPiece(state.board)) match {
      case (true, Some(targetPiece)) if targetPiece.team == currentPlayer.team.enemy =>
        Some(PlayerMove.TransformEnemyIntoAllyUnit(piece, targetPiece, moraleCost, AllyPieceData))
      case _ =>
        None
    }
  }

  case class MoveOrAttack(dx: Int, dy: Int) extends Moves {
    def getValidMove(piece: Piece, state: GameState, currentPlayer: Player): Option[PlayerMove] = {
      Seq(
        canMove(piece, piece.pos.translate(dx, dy), state, currentPlayer),
        canAttack(piece, piece.pos.translate(dx, dy), state, currentPlayer)
      ).flatten.headOption
    }
  }

  case class MoveOrAttackUnblockable(dx: Int, dy: Int) extends Moves {
    def getValidMove(piece: Piece, state: GameState, currentPlayer: Player): Option[PlayerMove] = {
      Seq(
        canMoveUnblockable(piece, piece.pos.translate(dx, dy), state, currentPlayer),
        canAttackUnblockable(piece, piece.pos.translate(dx, dy), state, currentPlayer)
      ).flatten.headOption
    }
  }

  case class MoveOrAttackOrSwapAlly(dx: Int, dy: Int) extends Moves {
    def getValidMove(piece: Piece, state: GameState, currentPlayer: Player): Option[PlayerMove] = {
      Seq(
        canMove(piece, piece.pos.translate(dx, dy), state, currentPlayer),
        canAttack(piece, piece.pos.translate(dx, dy), state, currentPlayer),
        canSwap(piece, piece.pos.translate(dx, dy), state, currentPlayer)
      ).flatten.headOption
    }
  }

  //  case class MoveOrAttackOrBlock1Attack(dx: Int, dy: Int) extends Moves

  case class Move(dx: Int, dy: Int) extends Moves {
    def getValidMove(piece: Piece, state: GameState, currentPlayer: Player): Option[PlayerMove] = {
      canMove(piece, piece.pos.translate(dx, dy), state, currentPlayer)
    }
  }

  case class MoveFromStart(dx: Int, dy: Int) extends Moves {
    def getValidMove(piece: Piece, state: GameState, currentPlayer: Player): Option[PlayerMove] = {
      canMoveFromStart(piece, piece.pos.translate(dx, dy), state, currentPlayer)
    }
  }

  case class MoveUnblockable(dx: Int, dy: Int) extends Moves {
    def getValidMove(piece: Piece, state: GameState, currentPlayer: Player): Option[PlayerMove] = {
      canMoveUnblockable(piece, piece.pos.translate(dx, dy), state, currentPlayer)
    }
  }

  case class Attack(dx: Int, dy: Int) extends Moves {
    def getValidMove(piece: Piece, state: GameState, currentPlayer: Player): Option[PlayerMove] = {
      canAttack(piece, piece.pos.translate(dx, dy), state, currentPlayer)
    }
  }

  case class RangedDestroy(dx: Int, dy: Int) extends Moves {
    def getValidMove(piece: Piece, state: GameState, currentPlayer: Player): Option[PlayerMove] = {
      canRangedDestroy(piece, piece.pos.translate(dx, dy), state, currentPlayer)
    }
  }

  case class MagicDestroy(dx: Int, dy: Int) extends Moves {
    def getValidMove(piece: Piece, state: GameState, currentPlayer: Player): Option[PlayerMove] = {
      canMagicDestroy(piece, piece.pos.translate(dx, dy), state, currentPlayer)
    }
  }

  case class RangedPetrify(dx: Int, dy: Int, durationTurns: Int) extends Moves {
    def getValidMove(piece: Piece, state: GameState, currentPlayer: Player): Option[PlayerMove] = {
      canRangedPetrify(piece, piece.pos.translate(dx, dy), durationTurns, state, currentPlayer)
    }
  }

  case class TaurusRush(dx: Int, dy: Int, maxDistance: Int) extends Moves {
    def getValidMove(piece: Piece, state: GameState, currentPlayer: Player): Option[PlayerMove] = {
      canTaurusRush(piece, piece.pos.translate(dx, dy), maxDistance, state, currentPlayer)
    }
  }

  case class TransformEnemyIntoAllyUnit(dx: Int, dy: Int, moraleCost: Int, allyUnitName: String) extends Moves {
    def getValidMove(piece: Piece, state: GameState, currentPlayer: Player): Option[PlayerMove] = {
      val allyUnitPieceData = DataLoader.getPieceData(allyUnitName, piece.data.team)
      canTransformEnemyIntoAllyUnit(piece, piece.pos.translate(dx, dy), moraleCost, allyUnitPieceData, state, currentPlayer)
    }
  }

  case class Castling(dx: Int, dy: Int, otherDx: Int, otherDy: Int) extends Moves {
    def getValidMove(piece: Piece, state: GameState, currentPlayer: Player): Option[PlayerMove] = {
      ???
    }
  }

  case object DummyMove extends Moves {
    val dx = 0
    val dy = 0

    def getValidMove(piece: Piece, state: GameState, currentPlayer: Player): Option[PlayerMove] = {
      Some(PlayerMove.DummyMove(piece))
    }
  }

}
