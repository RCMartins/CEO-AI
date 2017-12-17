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
      case (true, Some(targetPiece)) if targetPiece.team == currentPlayer.team.enemy =>
        Some(PlayerMove.Attack(piece, target.getPiece(state.board).get))
      case _ =>
        None
    }
  }

  private def canAttackUnblockable(piece: Piece, target: BoardPos, state: GameState, currentPlayer: Player): Option[PlayerMove] = {
    target.getPiece(state.board) match {
      case Some(targetPiece) if targetPiece.team == currentPlayer.team.enemy =>
        Some(PlayerMove.Attack(piece, target.getPiece(state.board).get))
      case _ =>
        None
    }
  }

  private def canSwap(piece: Piece, target: BoardPos, state: GameState, currentPlayer: Player): Option[PlayerMove] = {
    target.getPiece(state.board) match {
      case Some(targetPiece) if targetPiece.team == currentPlayer.team =>
        Some(PlayerMove.Attack(piece, target.getPiece(state.board).get))
      case _ =>
        None
    }
  }

  private def canRangedDestroy(piece: Piece, target: BoardPos, state: GameState, currentPlayer: Player): Option[PlayerMove] = {
    (piece.pos.posUntil(target).forall(_.isEmpty(state.board)), target.getPiece(state.board)) match {
      case (true, Some(targetPiece)) if targetPiece.team == currentPlayer.team.enemy =>
        Some(PlayerMove.RangedDestroy(piece, target.getPiece(state.board).get))
      case _ =>
        None
    }
  }

  private def canMagicDestroy(piece: Piece, target: BoardPos, state: GameState, currentPlayer: Player): Option[PlayerMove] = {
    target.getPiece(state.board) match {
      case Some(targetPiece) if targetPiece.team == currentPlayer.team.enemy =>
        Some(PlayerMove.MagicDestroy(piece, target.getPiece(state.board).get))
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
    (piece.pos.posUntil(target).forall(_.isEmpty(state.board)), target.getPiece(state.board)) match {
      case (true, Some(targetPiece)) if targetPiece.team == currentPlayer.team.enemy =>
        Some(PlayerMove.RangedPetrify(piece, target.getPiece(state.board).get))
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
    (piece.pos.posUntil(target).forall(_.isEmpty(state.board)), target.getPiece(state.board)) match {
      case (true, Some(targetPiece)) if targetPiece.team == currentPlayer.team.enemy =>
        Some(PlayerMove.TaurusRush(piece, target.getPiece(state.board).get, maxDistance))
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
        Some(PlayerMove.TransformEnemyIntoAllyUnit(piece, target.getPiece(state.board).get, moraleCost, AllyPieceData))
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

}
