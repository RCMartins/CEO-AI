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

  override def toString: String = name

  def createPiece(pos: BoardPos): Piece = Piece(this, pos, pos, initialMorale, effectStatus = List.empty)

  val simpleName: String = name.takeWhile(c => c.isLetter || c == '-')

  val tier: Int = name.count(_ == '+')

  def nameWithTier: String = s"$simpleName-$tier"

  def officialName: String = s"$simpleName${"+" * tier}"

  val isUnknown: Boolean = name.startsWith("?")

  val afterAnyDeathRunners: List[DynamicRunner[GameState, Piece /* this piece */ ]] = powers.collect {
    case OnAnyDeathPlayerChangeMorale(amount) => new DynamicRunner[GameState, Piece] {
      override def update(state: GameState, deathPiece: Piece): GameState = {
        state.changeMorale(team, amount)
      }
    }
    case TriggerGuardian(_) => new DynamicRunner[GameState, Piece] {
      override def update(state: GameState, deathPiece: Piece): GameState = {
        state.updatePlayer(state.getPlayer(team).updateGuardedPositions(Some(deathPiece), None))
      }
    }
    case OnDeathEnchantAdjacentChampions(enchantDuration) => new DynamicRunner[GameState, Piece] {
      override def update(startingState: GameState, deathPiece: Piece): GameState = {
        val currentPos = deathPiece.pos
        startingState.getPlayer(deathPiece.team).allPieces.filter(piece => piece.data.isChampion && piece.pos.distanceTo(currentPos) <= 1)
          .foldLeft(startingState) {
            (state, championPiece) =>
              val championPieceUpdated = championPiece.enchant(state, enchantDuration)
              state.updatePiece(championPiece, championPieceUpdated)
          }
      }
    }
    case OnDeathEnchantGlobalMinions(enchantDuration) => new DynamicRunner[GameState, Piece] {
      override def update(startingState: GameState, deathPiece: Piece): GameState = {
        startingState.getPlayer(deathPiece.team).allPieces.filter(_.data.isMinion).foldLeft(startingState) { (state, minionPiece) =>
          val minionPieceUpdated = minionPiece.enchant(state, enchantDuration)
          state.updatePiece(minionPiece, minionPieceUpdated)
        }
      }
    }
  }

  val afterMeleeDeathRunners: List[DynamicRunner[
    (GameState, Option[Piece] /* killer piece updated */ ),
    (Piece /* killer piece */ , Piece /* this piece */ )]] = powers.collect {
    case OnMeleeDeathKillAttacker => new DynamicRunner[(GameState, Option[Piece]), (Piece, Piece)] {
      override def update(state: (GameState, Option[Piece]), pieces: (Piece, Piece)): (GameState, Option[Piece]) = {
        state.copy(_2 = None)
      }
    }
    case OnMeleeDeathKillAttackerFromPosition(distances) => new DynamicRunner[(GameState, Option[Piece]), (Piece, Piece)] {
      override def update(state: (GameState, Option[Piece]), pieces: (Piece, Piece)): (GameState, Option[Piece]) = {
        if (distances.contains(pieces._1.pos - pieces._2.pos))
          state.copy(_2 = None)
        else
          state
      }
    }
    case OnMeleeDeathSpawnPieces(distances, pieceName) => new DynamicRunner[(GameState, Option[Piece]), (Piece, Piece)] {
      override def update(state: (GameState, Option[Piece]), pieces: (Piece, Piece)): (GameState, Option[Piece]) = {
        val pos = pieces._2.pos
        distances.foldLeft(state) { case ((gameState, updatedPiece), dist) =>
          val spawnPos = pos + dist
          if (spawnPos.isEmpty(gameState.board))
            (gameState.placePiece(DataLoader.getPieceData(pieceName, team).createPiece(spawnPos)), updatedPiece)
          else
            (gameState, updatedPiece)
        }
      }
    }
  }

  val afterKillRunners: List[DynamicRunner[
    (GameState, Option[Piece] /* killer piece updated */ ),
    Piece /* piece to kill */ ]] = powers.collect {
    case OnAnyKillSuicides => new DynamicRunner[(GameState, Option[Piece]), Piece] {
      override def update(state: (GameState, Option[Piece]), pieceToKill: Piece): (GameState, Option[Piece]) = {
        state.copy(_2 = None)
      }
    }
    case OnKillMercenary => new DynamicRunner[(GameState, Option[Piece]), Piece] {
      override def update(state: (GameState, Option[Piece]), pieceToKill: Piece): (GameState, Option[Piece]) = {
        if (pieceToKill.data.isChampion) {
          (state._1.changeMorale(team.enemy, -1), state._2.map(_.swapTeams))
        } else
          state
      }
    }
    case OnKillTransformInto(pieceName) => new DynamicRunner[(GameState, Option[Piece]), Piece] {
      override def update(state: (GameState, Option[Piece]), pieceToKill: Piece): (GameState, Option[Piece]) = {
        val updatedPiece = state._2.map(killerPiece => DataLoader.getPieceData(pieceName, team).createPiece(killerPiece.pos))
        (state._1, updatedPiece)
      }
    }
    case OnKillVampireAbility(moraleTakenFromEnemy, moraleToKing) => new DynamicRunner[(GameState, Option[Piece]), Piece] {
      override def update(state: (GameState, Option[Piece]), pieceToKill: Piece): (GameState, Option[Piece]) = {
        val (gameState1, maybePiece) = state
        val gameState2 = gameState1.changeMorale(team.enemy, -moraleTakenFromEnemy)

        val player = gameState2.getPlayer(team)
        val (gameState3, updatedPiece) =
          if (player.hasKing) {
            val king = player.allPieces.find(_.data.isKing).get
            val kingUpdated = king.changeMorale(moraleToKing)
            (gameState2.updatePiece(king, kingUpdated), maybePiece)
          } else {
            (gameState2, maybePiece.map(_.changeMorale(moraleToKing)))
          }

        (gameState3, updatedPiece)
      }
    }
  }

  val afterMagicCastRunners: List[DynamicRunner[
    (GameState, Option[Piece] /* piece updated */ ),
    Piece /* this piece */ ]] = powers.collect {
    case OnMagicCastDecayTo(decayAmount, limitToDevolve, pieceName) => new DynamicRunner[(GameState, Option[Piece]), Piece] {
      override def update(state: (GameState, Option[Piece]), thisPiece: Piece): (GameState, Option[Piece]) = {
        state._2 match {
          case None => state
          case Some(piece) =>
            val updatedPiece = piece.changeMorale(-decayAmount)
            if (updatedPiece.currentMorale == limitToDevolve) {
              state.copy(_2 = Some(DataLoader.getPieceData(pieceName, piece.team).createPiece(piece.pos)))
            } else {
              state.copy(_2 = Some(updatedPiece))
            }
        }
      }
    }
    case OnMagicCastDecayDeath(decayAmount) => new DynamicRunner[(GameState, Option[Piece]), Piece] {
      override def update(state: (GameState, Option[Piece]), thisPiece: Piece): (GameState, Option[Piece]) = {
        state._2 match {
          case None => state
          case Some(piece) =>
            val updatedPiece = piece.changeMorale(-decayAmount)
            if (updatedPiece.currentMorale == 0) {
              state.copy(_2 = None)
            } else {
              state.copy(_2 = Some(updatedPiece))
            }
        }
      }
    }
    case PromoteOnSpellCastTo(pieceName) => new DynamicRunner[(GameState, Option[Piece]), Piece] {
      override def update(state: (GameState, Option[Piece]), thisPiece: Piece): (GameState, Option[Piece]) = {
        state._2 match {
          case None => state
          case Some(piece) =>
            state.copy(_2 = Some(DataLoader.getPieceData(pieceName, piece.team).createPiece(piece.pos)))
        }
      }
    }
    case OnMagicVanish => new DynamicRunner[(GameState, Option[Piece]), Piece] {
      override def update(state: (GameState, Option[Piece]), thisPiece: Piece): (GameState, Option[Piece]) = {
        state.copy(_2 = None)
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

  val initialStatusEffects: List[EffectStatus] = {
    powers.collect {
      case BeginsGameEnchanted(enchantedDuration) =>
        EffectStatus.Enchanted(1 + enchantedDuration) //TODO check if the un-enchanted turn is correct!
      case BlockAttacksFrom(distances) =>
        EffectStatus.BlocksAttacksFrom(distances)
      case TriggerInstantKill(distance) =>
        EffectStatus.InstantKillPositional(distance)
    }
  }

  val canMinionPromote: Boolean = powers.exists {
    case PromoteTo(_) => true
    case _ => false
  }

  val hasUnstoppableMoves: Boolean = moves.exists {
    case Moves.UnstoppableTeleportTransformInto(_, _) => true
    case _ => false
  }

  val isABlockerPiece: Boolean = powers.exists {
    case BlockAttacksFrom(_) => true
    case _ => false
  }
}

object PieceData {

  val empty = PieceData("", isMinion = false, 0, Nil, Nil, PlayerTeam.White)

}
