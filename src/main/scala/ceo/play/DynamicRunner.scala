package ceo.play

import ceo.play.Powers._

import scala.collection.immutable.Seq

trait DynamicRunner[A, B] {

  def update(state: A, data: B): A

}

object DynamicRunner {
  def foldLeft[A, B](initialState: A, data: B, seq: Seq[DynamicRunner[A, B]]): A = {
    seq.foldLeft(initialState)((acc, runner) => runner.update(acc, data))
  }
}

trait SimpleRunner[A] extends DynamicRunner[A, Unit] {

  override def update(state: A, data: Unit): A = update(state)

  def update(value: A): A

}

case class GameRunner(
  globalPieceDeathRunners: List[DynamicRunner[GameState, Piece /* piece that died */ ]]
)

object GameRunner {
  val empty = GameRunner(Nil)

  def globalPieceDeathRunners(gameState: GameState): List[DynamicRunner[GameState, Piece /* piece that died */ ]] =
    gameState.allPieces.find(_.data.powers.contains(OnEnemyDeathMovesForward)) match {
      case None =>
        List.empty
      case Some(piece) =>
        val team = piece.team
        List(new DynamicRunner[GameState, Piece] {
          override def update(startingState: GameState, deadPiece: Piece): GameState = {
            if (deadPiece.team == team)
              startingState
            else {
              startingState.getPlayer(team).allPieces
                .filter(_.data.powers.contains(OnEnemyDeathMovesForward))
                .foldLeft(startingState) { (state, pieceToMove) =>
                  val forwardPos = pieceToMove.pos + startingState.getPlayer(team).directionForward
                  if (forwardPos.isEmpty(startingState.board)) {
                    state.playPlayerMove(PlayerMove.Move(pieceToMove, forwardPos), turnUpdate = false)
                  } else {
                    state
                  }
                }
            }
          }
        })
    }
}
