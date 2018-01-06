package ceo.play

import ceo.play.PlayerMove.MultiMove

abstract class PlayerMove(from: BoardPos, to: BoardPos) {

  self =>

  def getControllerMove: (BoardPos, BoardPos) = (from, to)

  def betterHumanString: String

  @inline def and(other: PlayerMove, realMoveFrom: BoardPos, realMoveTo: BoardPos): PlayerMove = MultiMove(self, other, realMoveFrom, realMoveTo)

}

object PlayerMove {

  case class MultiMove(
    move1: PlayerMove,
    move2: PlayerMove,
    realMoveFrom: BoardPos,
    realMoveTo: BoardPos
  ) extends PlayerMove(realMoveFrom, realMoveTo) {
    def betterHumanString: String = s"$move1 AND $move2"
  }

  case class Move(piece: Piece, to: BoardPos) extends PlayerMove(piece.pos, to) {
    def betterHumanString: String = s"$piece moves to $to   ${to - piece.pos}"
  }

  case class Attack(piece: Piece, pieceToKill: Piece) extends PlayerMove(piece.pos, pieceToKill.pos) {
    def betterHumanString: String = s"$piece attacks $pieceToKill   ${pieceToKill.pos - piece.pos}"
  }

  case class Swap(piece: Piece, pieceToSwap: Piece) extends PlayerMove(piece.pos, pieceToSwap.pos) {
    def betterHumanString: String = s"$piece swaps with $pieceToSwap   ${pieceToSwap.pos - piece.pos}"
  }

  case class RangedDestroy(piece: Piece, pieceToDestroy: Piece) extends PlayerMove(piece.pos, pieceToDestroy.pos) {
    def betterHumanString: String = s"$piece ranged-destroys $pieceToDestroy   ${pieceToDestroy.pos - piece.pos}"
  }

  case class MagicDestroy(piece: Piece, pieceToDestroy: Piece) extends PlayerMove(piece.pos, pieceToDestroy.pos) {
    def betterHumanString: String = s"$piece magic-destroys $pieceToDestroy   ${pieceToDestroy.pos - piece.pos}"
  }

  case class RangedPetrify(piece: Piece, pieceToPetrify: Piece, turnsPetrified: Int) extends PlayerMove(piece.pos, pieceToPetrify.pos) {
    def betterHumanString: String = s"$piece ranged-petrifies $pieceToPetrify " +
      s"for $turnsPetrified turns   ${pieceToPetrify.pos - piece.pos}"
  }

  case class MagicPoison(piece: Piece, pieceToPoison: Piece, turnsToDeath: Int) extends PlayerMove(piece.pos, pieceToPoison.pos) {
    def betterHumanString: String = s"$piece magic-poisoned $pieceToPoison " +
      s"killing it in $turnsToDeath turns   ${pieceToPoison.pos - piece.pos}"
  }

  case class TransformEnemyIntoAllyPiece(
    piece: Piece,
    pieceToTransform: Piece,
    moraleCost: Int,
    allyPieceData: PieceData
  ) extends PlayerMove(piece.pos, pieceToTransform.pos) {
    def betterHumanString: String =
      s"$piece transforms[$moraleCost cost] $pieceToTransform into ${allyPieceData.name}   ${pieceToTransform.pos - piece.pos}"
  }

  case class TaurusRush(piece: Piece, pieceToRush: Piece, maxDistance: Int) extends PlayerMove(piece.pos, pieceToRush.pos) {
    def betterHumanString: String = s"$piece RushEnemy $pieceToRush   ${pieceToRush.pos - piece.pos}"
  }

  case class KingDoesCastling(kingPiece: Piece, allyPiece: Piece, kingTarget: BoardPos, allyTarget: BoardPos) extends PlayerMove(kingPiece.pos, allyPiece.pos) {
    override def betterHumanString: String = s"$kingPiece does Castling with $allyPiece"
  }

  case class MagicCharm(piece: Piece, pieceToCharm: Piece) extends PlayerMove(piece.pos, pieceToCharm.pos) {
    def betterHumanString: String = s"$piece charms $pieceToCharm   ${pieceToCharm.pos - piece.pos}"
  }

  case class RangedPush(piece: Piece, pieceToPush: Piece, moraleCost: Int, maxPushDistance: Int) extends PlayerMove(piece.pos, pieceToPush.pos) {
    def betterHumanString: String = s"$piece ranged-pushes[$moraleCost cost] $pieceToPush " +
      s"for max $maxPushDistance distance   ${pieceToPush.pos - piece.pos}"
  }

  case class MagicPush(piece: Piece, pieceToPush: Piece, maxPushDistance: Int) extends PlayerMove(piece.pos, pieceToPush.pos) {
    def betterHumanString: String = s"$piece magic-pushes $pieceToPush " +
      s"for max $maxPushDistance distance   ${pieceToPush.pos - piece.pos}"
  }

  case class TeleportPiece(
    piece: Piece,
    pieceToTeleport: Piece,
    positionToTeleport: BoardPos,
    realMoveFrom: BoardPos,
    realMoveTo: BoardPos
  ) extends PlayerMove(realMoveFrom, realMoveTo) {
    def betterHumanString: String = s"$piece teleports $pieceToTeleport " +
      s"to $positionToTeleport   ${positionToTeleport - pieceToTeleport.pos}"
  }

  case class MagicFreeze(piece: Piece, pieceToFreeze: Piece, freezeDuration: Int) extends PlayerMove(piece.pos, pieceToFreeze.pos) {
    def betterHumanString: String = s"$piece magic-freezes $pieceToFreeze " +
      s"for $freezeDuration turns   ${pieceToFreeze.pos - piece.pos}"
  }

  case class MagicLightning(piece: Piece, lightningPosition: BoardPos, moraleCost: Int, durationTurns: Int) extends PlayerMove(piece.pos, lightningPosition) {
    def betterHumanString: String = s"$piece casts-lightning at $lightningPosition " +
      s"in $durationTurns turns   ${lightningPosition - piece.pos}"
  }

  case class TeleportTransformInto(piece: Piece, target: BoardPos, pieceData: PieceData) extends PlayerMove(piece.pos, target) {
    def betterHumanString: String = s"$piece fly-transforms into ${pieceData.name} " +
      s"at $target   ${target - piece.pos}"
  }

  case class DummyMove(piece: Piece) extends PlayerMove(???, ???) {
    def betterHumanString: String = s"$piece does nothing special"
  }

}