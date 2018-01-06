package ceo.play

import ceo.play.Powers._

case class PieceData(
  name: String,
  isMinion: Boolean,
  initialMorale: Int,
  moves: List[Moves],
  powers: List[Powers] = List.empty,
  team: PlayerTeam
) {

  override def toString: String = s"$name"

  def createPiece(pos: BoardPos): Piece = Piece(this, pos, pos, initialMorale, effectStatus = List.empty)

  val simpleName: String = name.takeWhile(c => c.isLetter || c == '-')

  val tier: Int = name.count(_ == '+')

  def nameWithTier: String = s"$simpleName-$tier"

  def officialName: String = s"$simpleName${"+" * tier}"

  val isUnknown: Boolean = name.startsWith("?")

  val afterKillRunners: List[DynamicRunner[(GameState, Piece), Piece]] = powers.collect {
    case OnKillMercenary => new DynamicRunner[(GameState, Piece), Piece] {
      override def update(value: (GameState, Piece), pieceToKill: Piece): (GameState, Piece) = {
        if (pieceToKill.data.isChampion) {
          (value._1.changeMorale(team.enemy, -1), value._2.swapTeams)
        } else
          value
      }
    }
    case OnKillTransformInto(pieceName) => new DynamicRunner[(GameState, Piece), Piece] {
      override def update(value: (GameState, Piece), pieceToKill: Piece): (GameState, Piece) = {
        val updatedPiece = DataLoader.getPieceData(pieceName, team).createPiece(value._2.pos)
        (value._1, updatedPiece)
      }
    }
    case OnKillVampireAbility(moraleTakenFromEnemy, moraleToKing) => new DynamicRunner[(GameState, Piece), Piece] {
      override def update(value: (GameState, Piece), pieceToKill: Piece): (GameState, Piece) = {
        val (state, piece) = value
        val state2 =
          state
            .changeMorale(piece.team.enemy, -moraleTakenFromEnemy)

        val player = state2.getPlayer(piece.team)
        val (state3, updatedPiece) =
          if (player.hasKing) {
            val king = player.allPieces.find(_.data.isKing).get
            val kingUpdated = king.changeMorale(moraleToKing)
            (state2.updatePiece(king, kingUpdated), piece)
          } else {
            (state2, piece.changeMorale(moraleToKing))
          }

        (state3, updatedPiece)
      }
    }
  }

  val isKing: Boolean = name.startsWith("King")

  val isRoyalty: Boolean =
    name.startsWith("King") || name.startsWith("Queen") || name.startsWith("Prince") || name.startsWith("Princess")

  val isChampion: Boolean = !isKing && !isMinion

  val isGhost: Boolean = powers.exists {
    case GhostMovement => true
    case _ => false
  }

  val isImmuneTo: Set[EffectType] = powers.flatMap {
    case ImmuneTo(list) => list
    case _ => List.empty
  }.toSet

  val isDestroyedBy: Set[EffectType] = powers.flatMap {
    case DestroyedBy(list) => list
    case _ => List.empty
  }.toSet

  val onAnyKillSuicides: Boolean = powers.exists {
    case OnAnyKillSuicides => true
    case _ => false
  }

  val hasOnMeleeDeathEffects: Boolean = powers.exists {
    case OnMeleeDeathKillAttacker | OnMeleeDeathSpawnPieces(_, _) => true
    case _ => false
  }

  val isGuardian: Boolean = powers.exists {
    case TriggerGuardian(_) => true
    case _ => false
  }

  val guardedPositions: Set[Distance] = powers.flatMap {
    case TriggerGuardian(distances) => distances
    case _ => List.empty
  }.toSet

  val canOnlyActAfterPieceLost: Boolean = powers.exists {
    case CanOnlyActAfterPieceLost => true
    case _ => false
  }

  val canMinionPromote: Boolean = powers.exists {
    case PromoteTo(_) => true
    case _ => false
  }

  val onMagicPromotes: Boolean = powers.exists {
    case PromoteOnSpellCastTo(_) => true
    case _ => false
  }

  val hasUnstoppableMoves: Boolean = moves.exists {
    case Moves.UnstoppableTeleportTransformInto(_, _) => true
    case _ => false
  }

}

object PieceData {

  val empty = PieceData("", isMinion = false, 0, Nil, Nil, PlayerTeam.White)

}
