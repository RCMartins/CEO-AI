package ceo.play

abstract class PlayerMove extends Ordered[PlayerMove] {

  self =>

  def getControllerMove: (BoardPos, BoardPos)

  def priorityHeuristic: Int

  def betterHumanString: String

  override def compare(that: PlayerMove): Int = that.priorityHeuristic - priorityHeuristic
}

object PlayerMove {

  case class Move(piece: Piece, to: BoardPos) extends PlayerMove {
    def getControllerMove: (BoardPos, BoardPos) = (piece.pos, to)

    def priorityHeuristic: Int = 0

    def betterHumanString: String = s"$piece moves to $to   ${to - piece.pos}"
  }

  case class Attack(piece: Piece, pieceToKill: Piece) extends PlayerMove {
    def getControllerMove: (BoardPos, BoardPos) = (piece.pos, pieceToKill.pos)

    def priorityHeuristic: Int = 100

    def betterHumanString: String = s"$piece attacks $pieceToKill   ${pieceToKill.pos - piece.pos}"
  }

  case class AttackCanBeBlocked(piece: Piece, pieceToAttack: Piece) extends PlayerMove {
    def getControllerMove: (BoardPos, BoardPos) = (piece.pos, pieceToAttack.pos)

    def priorityHeuristic: Int = 90

    def betterHumanString: String = s"$piece attack-blocked-by $pieceToAttack   ${pieceToAttack.pos - piece.pos}"
  }

  case class Swap(piece: Piece, pieceToSwap: Piece) extends PlayerMove {
    def getControllerMove: (BoardPos, BoardPos) = (piece.pos, pieceToSwap.pos)

    def priorityHeuristic: Int = 25

    def betterHumanString: String = s"$piece swaps with $pieceToSwap   ${pieceToSwap.pos - piece.pos}"
  }

  case class RangedDestroy(piece: Piece, pieceToDestroy: Piece) extends PlayerMove {
    def getControllerMove: (BoardPos, BoardPos) = (piece.pos, pieceToDestroy.pos)

    def priorityHeuristic: Int = 200

    def betterHumanString: String = s"$piece ranged-destroys $pieceToDestroy   ${pieceToDestroy.pos - piece.pos}"
  }

  case class MagicDestroy(piece: Piece, pieceToDestroy: Piece) extends PlayerMove {
    def getControllerMove: (BoardPos, BoardPos) = (piece.pos, pieceToDestroy.pos)

    def priorityHeuristic: Int = 200

    def betterHumanString: String = s"$piece magic-destroys $pieceToDestroy   ${pieceToDestroy.pos - piece.pos}"
  }

  case class RangedPetrify(piece: Piece, pieceToPetrify: Piece, turnsPetrified: Int) extends PlayerMove {
    def getControllerMove: (BoardPos, BoardPos) = (piece.pos, pieceToPetrify.pos)

    def priorityHeuristic: Int = 15

    def betterHumanString: String = s"$piece ranged-petrifies $pieceToPetrify " +
      s"for $turnsPetrified turns   ${pieceToPetrify.pos - piece.pos}"
  }

  case class MagicPoison(piece: Piece, pieceToPoison: Piece, turnsToDeath: Int) extends PlayerMove {
    def getControllerMove: (BoardPos, BoardPos) = (piece.pos, pieceToPoison.pos)

    def priorityHeuristic: Int = 20

    def betterHumanString: String = s"$piece magic-poisoned $pieceToPoison " +
      s"killing it in $turnsToDeath turns   ${pieceToPoison.pos - piece.pos}"
  }

  case class MagicPetrify(
    piece: Piece,
    pieceToPetrify: Piece,
    moraleCost: Int,
    turnsPetrified: Int
  ) extends PlayerMove {
    def getControllerMove: (BoardPos, BoardPos) = (piece.pos, pieceToPetrify.pos)

    def priorityHeuristic: Int = 15

    def betterHumanString: String = s"$piece magic-petrifies[$moraleCost cost] $pieceToPetrify " +
      s"for $turnsPetrified turns   ${pieceToPetrify.pos - piece.pos}"
  }

  case class TransformIntoAllyPiece(
    piece: Piece,
    pieceToTransform: Piece,
    moraleCost: Int,
    allyPieceData: PieceData
  ) extends PlayerMove {
    def getControllerMove: (BoardPos, BoardPos) = (piece.pos, pieceToTransform.pos)

    def priorityHeuristic: Int = 300

    def betterHumanString: String =
      s"$piece transforms[$moraleCost cost] $pieceToTransform into ${allyPieceData.name}   ${pieceToTransform.pos - piece.pos}"
  }

  case class TaurusRush(piece: Piece, pieceToRush: Piece, maxDistance: Int) extends PlayerMove {
    def getControllerMove: (BoardPos, BoardPos) = (piece.pos, pieceToRush.pos)

    def priorityHeuristic: Int = 80

    def betterHumanString: String = s"$piece RushEnemy $pieceToRush   ${pieceToRush.pos - piece.pos}"
  }

  case class KingCastling(kingPiece: Piece, allyPiece: Piece, kingTarget: BoardPos, allyTarget: BoardPos) extends PlayerMove {
    def getControllerMove: (BoardPos, BoardPos) = (kingPiece.pos, allyPiece.pos)

    def priorityHeuristic: Int = 25

    override def betterHumanString: String = s"$kingPiece does Castling with $allyPiece   ${allyPiece.pos - kingPiece.pos}"
  }

  case class MagicCharm(piece: Piece, pieceToCharm: Piece) extends PlayerMove {
    def getControllerMove: (BoardPos, BoardPos) = (piece.pos, pieceToCharm.pos)

    def priorityHeuristic: Int = 400

    def betterHumanString: String = s"$piece charms $pieceToCharm   ${pieceToCharm.pos - piece.pos}"
  }

  case class RangedPush(piece: Piece, pieceToPush: Piece, moraleCost: Int, maxPushDistance: Int) extends PlayerMove {
    def getControllerMove: (BoardPos, BoardPos) = (piece.pos, pieceToPush.pos)

    def priorityHeuristic: Int = 10

    def betterHumanString: String = s"$piece ranged-pushes[$moraleCost cost] $pieceToPush " +
      s"for max $maxPushDistance distance   ${pieceToPush.pos - piece.pos}"
  }

  case class MagicPush(piece: Piece, pieceToPush: Piece, moraleCost: Int, maxPushDistance: Int) extends PlayerMove {
    def getControllerMove: (BoardPos, BoardPos) = (piece.pos, pieceToPush.pos)

    def priorityHeuristic: Int = 10

    def betterHumanString: String = s"$piece magic-pushes${if (moraleCost == 0) "" else s"[$moraleCost cost]"} $pieceToPush " +
      s"for max $maxPushDistance distance   ${pieceToPush.pos - piece.pos}"
  }

  case class TeleportPiece(
    piece: Piece,
    pieceToTeleport: Piece,
    positionToTeleport: BoardPos,
    realMoveFrom: BoardPos,
    realMoveTo: BoardPos
  ) extends PlayerMove {
    def getControllerMove: (BoardPos, BoardPos) = (realMoveFrom, realMoveTo)

    def priorityHeuristic: Int = 0

    def betterHumanString: String = s"$piece teleports $pieceToTeleport " +
      s"to $positionToTeleport   ${positionToTeleport - pieceToTeleport.pos}"
  }

  case class MagicFreeze(piece: Piece, pieceToFreeze: Piece, freezeDuration: Int) extends PlayerMove {
    def getControllerMove: (BoardPos, BoardPos) = (piece.pos, pieceToFreeze.pos)

    def priorityHeuristic: Int = 15

    def betterHumanString: String = s"$piece magic-freezes $pieceToFreeze " +
      s"for $freezeDuration turns   ${pieceToFreeze.pos - piece.pos}"
  }

  case class MagicPushFreeze(piece: Piece, pieceToPushFreeze: Piece, maxPushDistance: Int, freezeDuration: Int) extends PlayerMove {
    def getControllerMove: (BoardPos, BoardPos) = (piece.pos, pieceToPushFreeze.pos)

    def priorityHeuristic: Int = 30

    def betterHumanString: String = s"$piece magic-push-freezes $pieceToPushFreeze " +
      s"for max $maxPushDistance distance and freezes for $freezeDuration turns   ${pieceToPushFreeze.pos - piece.pos}"
  }

  case class MagicLightning(piece: Piece, lightningPosition: BoardPos, moraleCost: Int, durationTurns: Int) extends PlayerMove {
    def getControllerMove: (BoardPos, BoardPos) = (piece.pos, lightningPosition)

    def priorityHeuristic: Int = -50

    def betterHumanString: String = s"$piece casts-lightning at $lightningPosition " +
      s"in $durationTurns turns   ${lightningPosition - piece.pos}"
  }

  case class TeleportTransformInto(piece: Piece, target: BoardPos, pieceData: PieceData) extends PlayerMove {
    def getControllerMove: (BoardPos, BoardPos) = (piece.pos, target)

    def priorityHeuristic: Int = 50

    def betterHumanString: String = s"$piece fly-transforms into $pieceData " +
      s"at $target   ${target - piece.pos}"
  }

  case class MagicSummonPiece(piece: Piece, target: BoardPos, moraleCost: Int, pieceData: PieceData) extends PlayerMove {
    def getControllerMove: (BoardPos, BoardPos) = (piece.pos, target)

    def priorityHeuristic: Int = 50

    def betterHumanString: String = s"$piece magic-summons[$moraleCost cost] $pieceData " +
      s"at $target   ${target - piece.pos}"
  }

  case class RangedSummonGeminiTwin(piece: Piece, target: BoardPos, moraleCost: Int, pieceData: PieceData) extends PlayerMove {
    def getControllerMove: (BoardPos, BoardPos) = (piece.pos, target)

    def priorityHeuristic: Int = 50

    def betterHumanString: String = s"$piece splits-into[$moraleCost cost] $pieceData " +
      s"at $target   ${target - piece.pos}"
  }

  case class MagicWeakEnchant(piece: Piece, pieceToWeakEnchant: Piece, durationTurns: Int) extends PlayerMove {
    def getControllerMove: (BoardPos, BoardPos) = (piece.pos, pieceToWeakEnchant.pos)

    def priorityHeuristic: Int = 10

    def betterHumanString: String = s"$piece magic-weak-enchants $pieceToWeakEnchant   ${pieceToWeakEnchant.pos - piece.pos}"
  }

  case class MagicEnvyClone(piece: Piece, pieceToClone: Piece) extends PlayerMove {
    def getControllerMove: (BoardPos, BoardPos) = (piece.pos, pieceToClone.pos)

    def priorityHeuristic: Int = 400

    def betterHumanString: String = s"$piece magic-clones $pieceToClone   ${pieceToClone.pos - piece.pos}"
  }

  case class MagicMeteor(
    piece: Piece,
    meteorPosition: BoardPos,
    moraleCost: Int,
    durationTurns: Int
  ) extends PlayerMove {
    def getControllerMove: (BoardPos, BoardPos) = (piece.pos, meteorPosition)

    def priorityHeuristic: Int = -50

    def betterHumanString: String = s"$piece casts-meteor[$moraleCost cost] at $meteorPosition " +
      s"in $durationTurns turns   ${meteorPosition - piece.pos}"
  }

  case class RangedPushPromoteTo(
    piece: Piece,
    pieceToPush: Piece,
    moraleCost: Int,
    maxPushDistance: Int,
    pieceData: PieceData
  ) extends PlayerMove {
    def getControllerMove: (BoardPos, BoardPos) = (piece.pos, pieceToPush.pos)

    def priorityHeuristic: Int = 50

    def betterHumanString: String = s"$piece ranged-pushes[$moraleCost cost] $pieceToPush " +
      s"for max $maxPushDistance distance and promotes to [$pieceData]   ${pieceToPush.pos - piece.pos}"
  }

  case class RangedPushSpawn(
    piece: Piece,
    pieceToPush: Piece,
    moraleCost: Int,
    maxPushDistance: Int,
    pieceData: PieceData
  ) extends PlayerMove {
    def getControllerMove: (BoardPos, BoardPos) = (piece.pos, pieceToPush.pos)

    def priorityHeuristic: Int = 50

    def betterHumanString: String = s"$piece ranged-pushes-spawns[$moraleCost cost] $pieceToPush " +
      s"for max $maxPushDistance distance [spawning $pieceData]   ${pieceToPush.pos - piece.pos}"
  }

  case class RangedCompel(
    piece: Piece,
    pieceToCompel: Piece,
    turnsCompelled: Int,
  ) extends PlayerMove {
    def getControllerMove: (BoardPos, BoardPos) = (piece.pos, pieceToCompel.pos)

    def priorityHeuristic: Int = 10

    def betterHumanString: String = s"$piece ranged-compels $pieceToCompel " +
      s"for $turnsCompelled turns   ${pieceToCompel.pos - piece.pos}"
  }

  case class MagicPushTowards(
    piece: Piece,
    pieceToPush: Piece,
    moraleCost: Int,
    maxPushDistance: Distance
  ) extends PlayerMove {
    def getControllerMove: (BoardPos, BoardPos) = (piece.pos, pieceToPush.pos)

    def priorityHeuristic: Int = 10

    def betterHumanString: String = s"$piece magic-pushes-towards[$moraleCost cost] $pieceToPush " +
      s"for max $maxPushDistance distance   ${pieceToPush.pos - piece.pos}"
  }

  case class MagicSuicideFreeze(piece: Piece, pieceToFreeze: Piece, freezeDuration: Int) extends PlayerMove {
    def getControllerMove: (BoardPos, BoardPos) = (piece.pos, pieceToFreeze.pos)

    def priorityHeuristic: Int = 15

    def betterHumanString: String = s"$piece magic-suicide-freezes $pieceToFreeze " +
      s"for $freezeDuration turns   ${pieceToFreeze.pos - piece.pos}"
  }

  case class DummyMove(piece: Piece) extends PlayerMove {
    def getControllerMove: (BoardPos, BoardPos) = (???, ???)

    def priorityHeuristic: Int = Int.MinValue

    def betterHumanString: String = s"$piece does nothing special"
  }

}