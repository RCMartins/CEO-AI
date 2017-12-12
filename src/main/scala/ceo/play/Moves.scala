package ceo.play

sealed trait Moves {
  def getValidMove(piece: Piece, state: GameState, currentPlayer: Player): Option[PlayerMove]

  def dx: Int

  def dy: Int
}

object Moves {

  private def canMove(piece: Piece, target: BoardPos, state: GameState, currentPlayer: Player): Option[PlayerMove] = {
    if (piece.pos.posTo(target).forall(_.isEmpty(state.board)))
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
    (piece.pos.posUntil(target).forall(_.isEmpty(state.board)), target.getPiece(state.board)) match {
      case (true, Some(targetPiece)) if targetPiece.team == currentPlayer.enemyColor =>
        Some(PlayerMove.Attack(piece, target.getPiece(state.board).get))
      case _ =>
        None
    }
  }

  private def canAttackUnblockable(piece: Piece, target: BoardPos, state: GameState, currentPlayer: Player): Option[PlayerMove] = {
    target.getPiece(state.board) match {
      case Some(targetPiece) if targetPiece.team == currentPlayer.enemyColor =>
        Some(PlayerMove.Attack(piece, target.getPiece(state.board).get))
      case _ =>
        None
    }
  }

  private def canSwap(piece: Piece, target: BoardPos, state: GameState, currentPlayer: Player): Option[PlayerMove] = {
    target.getPiece(state.board) match {
      case Some(targetPiece) if targetPiece.team == currentPlayer.color =>
        Some(PlayerMove.Attack(piece, target.getPiece(state.board).get))
      case _ =>
        None
    }
  }

  private def canRangedDestroy(piece: Piece, target: BoardPos, state: GameState, currentPlayer: Player): Option[PlayerMove] = {
    (piece.pos.posUntil(target).forall(_.isEmpty(state.board)), target.getPiece(state.board)) match {
      case (true, Some(targetPiece)) if targetPiece.team == currentPlayer.enemyColor =>
        Some(PlayerMove.RangedDestroy(piece, target.getPiece(state.board).get))
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

  case class Attack(dx: Int, dy: Int) extends Moves {
    def getValidMove(piece: Piece, state: GameState, currentPlayer: Player): Option[PlayerMove] = {
      canAttack(piece, piece.pos.translate(dx, dy), state, currentPlayer)
    }
  }

  //  case class AttackUnblockable(dx: Int, dy: Int) extends Moves
  //

  case class RangedDestroy(dx: Int, dy: Int) extends Moves {
    def getValidMove(piece: Piece, state: GameState, currentPlayer: Player): Option[PlayerMove] = {
      canRangedDestroy(piece, piece.pos.translate(dx, dy), state, currentPlayer)
    }
  }

  //
  //  case class MagicDestroy(dx: Int, dy: Int) extends Moves
  //
  //  case class Teleport(dx: Int, dy: Int) extends Moves

}
