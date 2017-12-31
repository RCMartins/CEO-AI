package ceo.play

import ceo.play.PlayerMove.MultiMove

sealed trait PlayerMove {

  self =>

  def betterHumanString: String

  @inline def and(other: PlayerMove): PlayerMove = MultiMove(self, other)

}

object PlayerMove {

  case class MultiMove(move1: PlayerMove, move2: PlayerMove) extends PlayerMove {
    def betterHumanString: String = s"$move1 AND $move2"
  }

  case class Move(piece: Piece, to: BoardPos) extends PlayerMove {
    def betterHumanString: String = s"$piece moves to $to   ${to - piece.pos}"
  }

  case class Attack(piece: Piece, pieceToKill: Piece) extends PlayerMove {
    def betterHumanString: String = s"$piece attacks $pieceToKill   ${pieceToKill.pos - piece.pos}"
  }

  case class Swap(piece: Piece, pieceToSwap: Piece) extends PlayerMove {
    def betterHumanString: String = s"$piece swaps with $pieceToSwap   ${pieceToSwap.pos - piece.pos}"
  }

  case class RangedDestroy(piece: Piece, pieceToDestroy: Piece) extends PlayerMove {
    def betterHumanString: String = s"$piece ranged-destroys $pieceToDestroy   ${pieceToDestroy.pos - piece.pos}"
  }

  case class MagicDestroy(piece: Piece, pieceToDestroy: Piece) extends PlayerMove {
    def betterHumanString: String = s"$piece magic-destroys $pieceToDestroy   ${pieceToDestroy.pos - piece.pos}"
  }

  case class RangedPetrify(piece: Piece, pieceToPetrify: Piece, turnsPetrified: Int) extends PlayerMove {
    def betterHumanString: String = s"$piece ranged-petrifies $pieceToPetrify " +
      s"for $turnsPetrified turns   ${pieceToPetrify.pos - piece.pos}"
  }

  case class MagicPoison(piece: Piece, pieceToPoison: Piece, turnsToDeath: Int) extends PlayerMove {
    def betterHumanString: String = s"$piece magic-poisoned $pieceToPoison " +
      s"killing it in $turnsToDeath turns   ${pieceToPoison.pos - piece.pos}"
  }

  case class TransformEnemyIntoAllyPiece(
    piece: Piece,
    pieceToTransform: Piece,
    moraleCost: Int,
    allyPieceData: PieceData
  ) extends PlayerMove {
    def betterHumanString: String =
      s"$piece transforms[$moraleCost cost] $pieceToTransform into ${allyPieceData.name}   ${pieceToTransform.pos - piece.pos}"
  }

  case class TaurusRush(piece: Piece, pieceToRush: Piece, maxDistance: Int) extends PlayerMove {
    def betterHumanString: String = s"$piece RushEnemy $pieceToRush   ${pieceToRush.pos - piece.pos}"
  }

  case class KingDoesCastling(kingPiece: Piece, allyPiece: Piece, kingTarget: BoardPos, allyTarget: BoardPos) extends PlayerMove {
    override def betterHumanString: String = s"$kingPiece does Castling with $allyPiece"
  }

  case class MagicCharm(piece: Piece, pieceToCharm: Piece) extends PlayerMove {
    def betterHumanString: String = s"$piece charms $pieceToCharm   ${pieceToCharm.pos - piece.pos}"
  }

  case class RangedPush(piece: Piece, pieceToPush: Piece, moraleCost: Int, maxPushDistance: Int) extends PlayerMove {
    def betterHumanString: String = s"$piece ranged-pushes[$moraleCost cost] $pieceToPush " +
      s"for max $maxPushDistance distance   ${pieceToPush.pos - piece.pos}"
  }

  case class MagicPush(piece: Piece, pieceToPush: Piece, maxPushDistance: Int) extends PlayerMove {
    def betterHumanString: String = s"$piece magic-pushes $pieceToPush " +
      s"for max $maxPushDistance distance   ${pieceToPush.pos - piece.pos}"
  }

  case class TeleportPiece(piece: Piece, pieceToTeleport: Piece, positionToTeleport: BoardPos) extends PlayerMove {
    def betterHumanString: String = s"$piece teleports $pieceToTeleport " +
      s"to $positionToTeleport   ${positionToTeleport - pieceToTeleport.pos}"
  }

  case class MagicFreeze(piece: Piece, pieceToFreeze: Piece, freezeDuration: Int) extends PlayerMove {
    def betterHumanString: String = s"$piece magic-freezes $pieceToFreeze " +
      s"for $freezeDuration turns   ${pieceToFreeze.pos - piece.pos}"
  }

  case class MagicLightning(piece: Piece, lightningPosition: BoardPos, moraleCost: Int, durationTurns: Int) extends PlayerMove {
    def betterHumanString: String = s"$piece casts-lightning at $lightningPosition " +
      s"in $durationTurns turns   ${lightningPosition - piece.pos}"
  }

  case class DummyMove(piece: Piece) extends PlayerMove {
    def betterHumanString: String = s"$piece does nothing special"
  }

}