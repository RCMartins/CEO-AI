package ceo.play

import java.util.concurrent.atomic.AtomicInteger

import ceo.play.Powers._
import com.softwaremill.quicklens._

case class PieceData(
  name: String,
  isMinion: Boolean,
  isChampion: Boolean,
  isExtra: Boolean,
  initialMorale: Int,
  moves: List[Moves],
  powers: List[Powers] = List.empty,
  team: PlayerTeam
) {

  override def toString: String = name

  def createPiece(pos: BoardPos): Piece = Piece(this, pos, pos, initialMorale, effectStatus = initialStatusEffects)

  val simpleName: String = name.takeWhile(c => c != '+' && c != '_')

  val tier: Int = name.count(_ == '+')

  def nameWithTier: String = s"$simpleName-$tier"

  def officialName: String = s"$simpleName${"+" * tier}"

  def nameWithPlayerBase: String = s"${officialName}_$team"

  def officialNameForReplay: String = s"${officialName}_${team.letter}"

  val isUnknown: Boolean = name.startsWith("?")

  val afterAnyDeathRunners: List[DynamicRunner[
    (GameState, Option[Piece] /* killer piece updated */ ),
    Piece /* this piece */ ]] = powers.collect {
    case OnAnyDeathPlayerChangeMorale(amount) => new DynamicRunner[(GameState, Option[Piece]), Piece] {
      override def update(state: (GameState, Option[Piece]), deathPiece: Piece): (GameState, Option[Piece]) = {
        modify(state)(_._1).using(_.changeMorale(team, amount))
      }
    }
    case TriggerGuardian(_) => new DynamicRunner[(GameState, Option[Piece]), Piece] {
      override def update(state: (GameState, Option[Piece]), deathPiece: Piece): (GameState, Option[Piece]) = {
        modify(state)(_._1).using(state =>
          state.updatePlayer(state.getPlayer(team).updateGuardedPositions(Some(deathPiece), None))
        )
      }
    }
    case OnDeathEnchantAdjacentChampions(enchantDuration) => new DynamicRunner[(GameState, Option[Piece]), Piece] {
      override def update(state: (GameState, Option[Piece]), deathPiece: Piece): (GameState, Option[Piece]) = {
        modify(state)(_._1).using { startingState =>
          val currentPos = deathPiece.pos
          startingState.getPlayer(deathPiece.team).allPieces.filter(piece => piece.data.isChampion && piece.pos.distanceTo(currentPos) <= 1)
            .foldLeft(startingState) {
              (currentState, championPiece) =>
                val (_, championPieceUpdated) = deathPiece.enchantPiece(currentState, championPiece, enchantDuration)
                currentState.updatePieceIfAlive(championPiece, championPieceUpdated)
            }
        }
      }
    }
    case OnDeathEnchantGlobalMinions(enchantDuration) => new DynamicRunner[(GameState, Option[Piece]), Piece] {
      override def update(state: (GameState, Option[Piece]), deathPiece: Piece): (GameState, Option[Piece]) = {
        modify(state)(_._1).using { startingState =>
          startingState.getPlayer(deathPiece.team).allPieces.filter(_.data.isMinion).foldLeft(startingState) {
            (currentState, minionPiece) =>
              val (_, minionPieceUpdated) = deathPiece.enchantPiece(currentState, minionPiece, enchantDuration + 0.5)
              currentState.updatePieceIfAlive(minionPiece, minionPieceUpdated)
          }
        }
      }
    }
    case OnDeathHostageChangesMorale(moraleAmount) => new DynamicRunner[(GameState, Option[Piece]), Piece] {
      override def update(state: (GameState, Option[Piece]), deathPiece: Piece): (GameState, Option[Piece]) = {
        modify(state)(_._1).using { gameState =>
          // Bug simulation of v0.52:
          // if the white player kills a hostage loses 6 morale, but if the black player kills the hostage loses 8 morale...

          if (deathPiece.team.color == PlayerColor.Black) {
            gameState.changeMorale(deathPiece.team.enemy, moraleAmount + deathPiece.data.powers.collectFirst { case HostageCaught(caughtAmount) => caughtAmount }.get)
          } else {
            gameState.changeMorale(deathPiece.team.enemy, moraleAmount)
          }
        }
      }
    }
    case TriggerFrostMephit(freezeDuration) => new DynamicRunner[(GameState, Option[Piece]), Piece] {
      override def update(state: (GameState, Option[Piece]), deathPiece: Piece): (GameState, Option[Piece]) = {
        modify(state)(_._2).using(_.map {
          case attackerPiece if attackerPiece.data.isImmuneTo(EffectType.Freeze) => attackerPiece
          case attackerPiece => attackerPiece.addEffect(EffectStatus.Frozen(state._1.currentTurn + freezeDuration))
        })
      }
    }
    case WispReflect => new DynamicRunner[(GameState, Option[Piece]), Piece] {
      override def update(state: (GameState, Option[Piece]), deathPiece: Piece): (GameState, Option[Piece]) = {
        state._2 match {
          case None =>
            state
          case Some(attackerPiece) =>
            (attackerPiece.onTransform(state._1), None)
        }
      }
    }
    case OnDeathPhoenix(eggPieceName) => new DynamicRunner[(GameState, Option[Piece]), Piece] {
      override def update(state: (GameState, Option[Piece]), deathPiece: Piece): (GameState, Option[Piece]) = {
        modify(state)(_._1).using(
          _.addEndOfTurnPiece(DataLoader.getPieceData(eggPieceName, deathPiece.team).createPiece(deathPiece.pos))
        )
      }
    }
    case OnDeathHalfMoraleToKing => new DynamicRunner[(GameState, Option[Piece]), Piece] {
      override def update(state: (GameState, Option[Piece]), deathPiece: Piece): (GameState, Option[Piece]) = {
        modify(state)(_._1).using { gameState =>
          val player = gameState.getPlayer(deathPiece.team)
          player.allPieces.find(_.data.isKing) match {
            case None =>
              gameState
            case Some(kingPiece) =>
              gameState.updatePiece(kingPiece, kingPiece.changeMorale(Math.floor(deathPiece.currentMorale / 2).toInt))
          }
        }
      }
    }
    case OnDeathTriggerFreezeMinions(freezeDistances, freezeDuration) => new DynamicRunner[(GameState, Option[Piece]), Piece] {
      override def update(state: (GameState, Option[Piece]), deathPiece: Piece): (GameState, Option[Piece]) = {
        val team = deathPiece.team
        modify(state)(_._1).using { initialState =>
          val pos = deathPiece.pos
          freezeDistances.map(_ + pos).foldLeft(initialState) { case (gameState, boardPos) =>
            boardPos.getPiece(gameState.board) match {
              case Some(enemyPiece) if enemyPiece.team != team && enemyPiece.data.isMinion =>
                val (_, enemyPieceUpdated) = deathPiece.freezePiece(gameState, enemyPiece, freezeDuration)
                gameState.updatePieceIfAlive(enemyPiece, enemyPieceUpdated)
              case _ =>
                gameState
            }
          }
        }
      }
    }
    case OnDeathAdjacentEnemiesFreezePush(pushDistance, freezeDuration) => new DynamicRunner[(GameState, Option[Piece]), Piece] {
      override def update(state: (GameState, Option[Piece]), deathPiece: Piece): (GameState, Option[Piece]) = {
        modify(state)(_._1).using(
          _.addEndOfTurnAction(EndOfTurnAction.AquariusExploded(deathPiece, pushDistance, freezeDuration))
        )
      }
    }
  }

  val afterMeleeDeathRunners: List[DynamicRunner[
    (GameState, Option[Piece] /* killer piece updated */ ),
    (Piece /* original killer piece */ , Piece /* this piece */ )]] = powers.collect {
    case OnMeleeDeathKillAttacker | OnDeathPhoenix(_) => new DynamicRunner[(GameState, Option[Piece]), (Piece, Piece)] {
      override def update(state: (GameState, Option[Piece]), pieces: (Piece, Piece)): (GameState, Option[Piece]) = {
        state._2 match {
          case None =>
            state
          case Some(attackerPiece) =>
            (attackerPiece.onTransform(state._1), None)
        }
      }
    }
    case OnMeleeDeathKillAttackerFromPosition(distances) => new DynamicRunner[(GameState, Option[Piece]), (Piece, Piece)] {
      override def update(state: (GameState, Option[Piece]), pieces: (Piece, Piece)): (GameState, Option[Piece]) = {
        state._2 match {
          case Some(attackerPiece) if distances.contains(pieces._1.pos - pieces._2.pos) =>
            (attackerPiece.onTransform(state._1), None)
          case _ =>
            state
        }
      }
    }
    case OnMeleeDeathSpawnSlimes(distances, pieceName) => new DynamicRunner[(GameState, Option[Piece]), (Piece, Piece)] {
      override def update(state: (GameState, Option[Piece]), pieces: (Piece, Piece)): (GameState, Option[Piece]) = {
        if (pieces._2.effectStatus.exists(effect => effect.effectType == EffectType.Freeze || effect.effectType == EffectType.Petrify))
          state
        else {
          val slimeData = DataLoader.getPieceData(pieceName, team)
          val pos = pieces._2.pos
          distances.foldLeft(state) { case ((gameState, updatedPiece), dist) =>
            val spawnPosition = pos + dist
            (gameState.addEndOfTurnPiece(slimeData.createPiece(spawnPosition)), updatedPiece)
          }
        }
      }
    }
    case OnMeleeDeathTriggerRevive(distance, moraleMinimum) => new DynamicRunner[(GameState, Option[Piece]), (Piece, Piece)] {
      override def update(state: (GameState, Option[Piece]), pieces: (Piece, Piece)): (GameState, Option[Piece]) = {
        val deadPiece = pieces._2
        val spawnPosition = deadPiece.pos + distance
        val updatedMorale = deadPiece.currentMorale - moraleMinimum
        if (updatedMorale >= 0 && spawnPosition.isEmpty(state._1.board))
          modify(state)(_._1).using(
            _.addEndOfTurnPiece(deadPiece.data.createPiece(spawnPosition).setMorale(updatedMorale))
          )
        else
          state
      }
    }
    case OnMeleeDeathPoisonIfMoraleLess(maxMoraleToPoison, turnsToDeath) => new DynamicRunner[(GameState, Option[Piece]), (Piece, Piece)] {
      override def update(state: (GameState, Option[Piece]), pieces: (Piece, Piece)): (GameState, Option[Piece]) = {
        val originalAttackerPiece = pieces._1
        modify(state)(_._2).using(_.map {
          case attackerPiece if {
            originalAttackerPiece.currentMorale <= maxMoraleToPoison &&
              !originalAttackerPiece.data.isImmuneTo(EffectType.Poison)
          } =>
            attackerPiece.addEffect(EffectStatus.Poison(state._1.currentTurn + turnsToDeath))
          case attackerPiece =>
            attackerPiece
        })
      }
    }
  }

  val afterKillRunners: List[DynamicRunner[
    (GameState, Option[Piece] /* killer piece updated */ ),
    Piece /* piece to kill */ ]] = powers.collect {
    case OnAnyKillSuicides => new DynamicRunner[(GameState, Option[Piece]), Piece] {
      override def update(state: (GameState, Option[Piece]), pieceToKill: Piece): (GameState, Option[Piece]) = {
        state._2 match {
          case None =>
            state
          case Some(attackerPiece) =>
            (attackerPiece.onTransform(state._1), None)
        }
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
        modify(state)(_._2).using(
          _.map(killerPiece => DataLoader.getPieceData(pieceName, team).createPiece(killerPiece.pos))
        )
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
    case OnKillPromoteToKing(moraleBonus) => new DynamicRunner[(GameState, Option[Piece]), Piece] {
      override def update(state: (GameState, Option[Piece]), pieceToKill: Piece): (GameState, Option[Piece]) = {
        state._2 match {
          case None => state
          case Some(killerPiece) if state._1.getPlayer(killerPiece.team).hasKing => state
          case Some(killerPiece) =>
            val newPiece =
              DataLoader.getPieceData("King", team)
                .createPiece(killerPiece.pos)
            (state._1.changeMorale(killerPiece.team, moraleBonus), Some(newPiece))
        }
      }
    }
    case OnChampionKillSwapEnemyKing => new DynamicRunner[(GameState, Option[Piece]), Piece] {
      override def update(state: (GameState, Option[Piece]), pieceToKill: Piece): (GameState, Option[Piece]) = {
        if (pieceToKill.data.isChampion) {
          state._2 match {
            case None => state
            case Some(_) =>
              modify(state)(_._1).using(
                _.addEndOfTurnAction(EndOfTurnAction.PieceSwapWithEnemyKing(pieceToKill.pos))
              )
          }
        } else {
          state
        }
      }
    }
    case OnKillPieceGainMorale(moraleAmount) => new DynamicRunner[(GameState, Option[Piece]), Piece] {
      override def update(state: (GameState, Option[Piece]), pieceToKill: Piece): (GameState, Option[Piece]) = {
        modify(state)(_._2).using(
          _.map(_.changeMorale(moraleAmount))
        )
      }
    }
    case OnKillPlayerChangeMorale(moraleAmount) => new DynamicRunner[(GameState, Option[Piece]), Piece] {
      override def update(state: (GameState, Option[Piece]), pieceToKill: Piece): (GameState, Option[Piece]) = {
        modify(state)(_._1).using(
          _.changeMorale(team, moraleAmount)
        )
      }
    }
    case OnKillDecayTo(moraleLostOnKill, moraleLimit, pieceName) => new DynamicRunner[(GameState, Option[Piece]), Piece] {
      override def update(state: (GameState, Option[Piece]), pieceToKill: Piece): (GameState, Option[Piece]) = {
        modify(state)(_._2).using(
          _.map { piece =>
            val pieceUpdated = piece.changeMorale(-moraleLostOnKill)
            if (pieceUpdated.currentMorale <= moraleLimit) {
              DataLoader.getPieceData(pieceName, team).createPiece(piece.pos)
            } else {
              pieceUpdated
            }
          }
        )
      }
    }
    case OnKillGainHalfMorale => new DynamicRunner[(GameState, Option[Piece]), Piece] {
      override def update(state: (GameState, Option[Piece]), pieceToKill: Piece): (GameState, Option[Piece]) = {
        modify(state)(_._2).using(
          _.map(_.changeMorale(Math.floor(pieceToKill.currentMorale / 2).toInt))
        )
      }
    }
  }

  val afterMagicCastRunners: List[DynamicRunner[
    (GameState, Option[Piece] /* piece updated */ ),
    (Piece /* this piece */ , Option[Piece] /* piece attacked by magic */ )]] = powers.collect {
    case OnMagicCastDecayTo(decayAmount, limitToDevolve, pieceName) => new DynamicRunner[(GameState, Option[Piece]), (Piece, Option[Piece])] {
      override def update(state: (GameState, Option[Piece]), data: (Piece, Option[Piece])): (GameState, Option[Piece]) = {
        modify(state)(_._2).using {
          case None => None
          case Some(piece) =>
            val updatedPiece = piece.changeMorale(-decayAmount)
            if (updatedPiece.currentMorale <= limitToDevolve) {
              Some(DataLoader.getPieceData(pieceName, piece.team).createPiece(piece.pos))
            } else {
              Some(updatedPiece)
            }
        }
      }
    }
    case OnMagicCastDecayDeath(decayAmount) => new DynamicRunner[(GameState, Option[Piece]), (Piece, Option[Piece])] {
      override def update(state: (GameState, Option[Piece]), data: (Piece, Option[Piece])): (GameState, Option[Piece]) = {
        modify(state)(_._2).using {
          case None => None
          case Some(piece) =>
            val updatedPiece = piece.changeMorale(-decayAmount)
            if (updatedPiece.currentMorale <= 0) {
              None
            } else {
              Some(updatedPiece)
            }
        }
      }
    }
    case OnMagicCastPromoteTo(pieceName) => new DynamicRunner[(GameState, Option[Piece]), (Piece, Option[Piece])] {
      override def update(state: (GameState, Option[Piece]), data: (Piece, Option[Piece])): (GameState, Option[Piece]) = {
        modify(state)(_._2).using {
          _.map(piece =>
            DataLoader.getPieceData(pieceName, piece.team).createPiece(piece.pos)
          )
        }
      }
    }
    case OnMagicVanish => new DynamicRunner[(GameState, Option[Piece]), (Piece, Option[Piece])] {
      override def update(state: (GameState, Option[Piece]), data: (Piece, Option[Piece])): (GameState, Option[Piece]) = {
        state.copy(_2 = None)
      }
    }
    case OnMagicCastPromoteIfEnemy(pieceName) => new DynamicRunner[(GameState, Option[Piece]), (Piece, Option[Piece])] {
      override def update(state: (GameState, Option[Piece]), data: (Piece, Option[Piece])): (GameState, Option[Piece]) = {
        modify(state)(_._2).using {
          _.map(piece =>
            data._2 match {
              case Some(attackedPiece) if attackedPiece.team != data._1.team =>
                DataLoader.getPieceData(pieceName, piece.team).createPiece(piece.pos)
              case _ =>
                piece
            }
          )
        }
      }
    }
  }

  val attackerUsesStatusEffectRunners: List[DynamicRunner[
    Option[Piece] /* attacker piece updated */ ,
    (GameState, EffectStatus)]] = powers.collect {
    case TriggerFrostMephit(freezeDuration) => new DynamicRunner[Option[Piece], (GameState, EffectStatus)] {
      override def update(attackerPieceOption: Option[Piece], data: (GameState, EffectStatus)): Option[Piece] = {
        attackerPieceOption.map {
          case attackerPiece if attackerPiece.data.isImmuneTo(EffectType.Freeze) => attackerPiece
          case attackerPiece => attackerPiece.addEffect(EffectStatus.Frozen(data._1.currentTurn + freezeDuration))
        }
      }
    }
    case WispReflect => new DynamicRunner[Option[Piece], (GameState, EffectStatus)] {
      override def update(attackerPieceOption: Option[Piece], data: (GameState, EffectStatus)): Option[Piece] = {
        val effectStatus = data._2
        attackerPieceOption.map {
          case attackerPiece if attackerPiece.data.isImmuneTo(effectStatus.effectType) => attackerPiece
          case attackerPiece => attackerPiece.addEffect(effectStatus)
        }
      }
    }
  }

  val afterPieceMovesRunners: List[DynamicRunner[
    (GameState, Option[Piece] /* piece that moved updated */ ),
    (Piece /* original piece that moved */ , Distance /* distance of the move */ , Boolean /* from a player move */ )]] = powers.collect {
    case OnMoveAdjacentHoplitesMove => new DynamicRunner[(GameState, Option[Piece]), (Piece, Distance, Boolean)] {
      override def update(state: (GameState, Option[Piece]), data: (Piece, Distance, Boolean)): (GameState, Option[Piece]) = {
        val isFromPlayerMove = data._3
        if (!isFromPlayerMove) {
          state
        } else {
          val piecePosAfterMove = data._1.pos
          val distance = data._2
          val center = piecePosAfterMove - distance
          state.copy(_1 = {
            val adjacentDistances = if (team.isTop) Distance.adjacentDistancesReversed else Distance.adjacentDistances
            adjacentDistances.map(dist => (dist + center).getPiece(state._1.board)).foldLeft(state._1) {
              case (gameState, Some(piece)) if piece.team == team && piece.pos != piecePosAfterMove && piece.data.isHoplite =>
                val (updatedGameState, updatePiece) = piece.moveTo(gameState, piece.pos + distance)
                updatedGameState
                  .removePiece(piece)
                  .doActionIfCondition(updatePiece.isDefined, _.addEndOfTurnPiece(updatePiece.get))
              case (gameState, _) =>
                gameState
            }
          })
        }
      }
    }
    case HostageCaught(moraleAmount) => new DynamicRunner[(GameState, Option[Piece]), (Piece, Distance, Boolean)] {
      val possibleDistancePairs: List[(Distance, Distance)] =
        List(Distance(-1, -1), Distance(-1, 0), Distance(-1, +1), Distance(0, -1)).map(dist => (dist, dist * -1))

      override def update(state: (GameState, Option[Piece]), data: (Piece, Distance, Boolean)): (GameState, Option[Piece]) = {
        val hostageOption = state._2
        hostageOption match {
          case Some(hostage) =>
            val pos = hostage.pos
            val board = state._1.board
            val enemyTeam = hostage.team.enemy
            val hostageCaught =
              possibleDistancePairs.exists { case (dist1, dist2) =>
                (dist1 + pos).getPiece(board).exists(piece => piece.team == enemyTeam && piece.data.isChampion) &&
                  (dist2 + pos).getPiece(board).exists(piece => piece.team == enemyTeam && piece.data.isChampion)
              }
            if (hostageCaught)
              (state._1.changeMorale(enemyTeam, moraleAmount), None)
            else
              state
          case _ =>
            state
        }
      }
    }
  }

  val isKing: Boolean = name.startsWith("King")

  val isRoyalty: Boolean =
    name.startsWith("King") || name.startsWith("Queen") || name.startsWith("Prince") || name.startsWith("Princess")

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
      case TriggerInstantKill(_) =>
        EffectStatus.InstantKillPositional
      case GrowMoraleOnPlayerTurnUntilTransform(moraleToPromote, pieceName) =>
        EffectStatus.PieceGrowOnPlayerTurn(moraleToPromote, pieceName)
      case GrowMoraleUntilTransform(moraleToPromote, pieceName) =>
        EffectStatus.PieceGrowOnOpponentTurn(moraleToPromote, pieceName)
      case DecayAfterTurn(turnStarts, moralePerTurn) =>
        EffectStatus.DecayAfterTurn(turnStarts, moralePerTurn)
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

  val cannotBeTargetedByMinions: Boolean = powers.exists {
    case CannotBeTargetedByMinions => true
    case _ => false
  }

  val isHoplite: Boolean = powers.exists {
    case OnMoveAdjacentHoplitesMove => true
    case _ => false
  }

  val isDummyPiece: Boolean = powers.exists {
    case Dummy => true
    case _ => false
  }

  val isDove: Boolean = powers.exists {
    case OnEnemyDeathMovesForward => true
    case _ => false
  }

  val isNecromancer: Boolean = simpleName == "Necromancer"

  val isSkeleton: Boolean = simpleName == "Skeleton"

  val isBonePile: Boolean = simpleName == "BonePile"

  val isSamurai: Boolean = powers.exists {
    case TriggerInstantKill(_) => true
    case _ => false
  }

  val hash: Long = {
    PieceData.atomicInt.getAndIncrement()
  }

  val canChangeStatusEffects: Boolean = {
    moves.exists {
      case _: Moves.RangedPetrify => true
      case _: Moves.RangedCompel => true
      case _: Moves.MagicFreezePiece => true
      case _: Moves.MagicPushFreezePiece => true
      case _: Moves.MagicDestroySelfButterfly => true
      case _: Moves.MagicDestroySelfAquarius => true
      case _: Moves.MagicFreezeStrikeOnEnemyChampions => true
      case _: Moves.MagicLightning => true
      case _: Moves.MagicMeteor => true
      case _: Moves.MagicPoison => true
      case _: Moves.MagicStonePillar => true
      case _: Moves.MagicWeakEnchant => true
      case _ => false
    } || powers.exists {
      case _: Powers.OnKillVampireAbility => true
      case _: Powers.BeginsGameEnchanted => true
      case _: Powers.DecayAfterTurn => true
      case _: Powers.HostageCaught => true
      case Powers.WispReflect => true
      case _: Powers.OnDeathPhoenix => true
      case _ => false
    }
  }
}

object PieceData {

  private val atomicInt = new AtomicInteger(0)

  val empty = PieceData("", isMinion = false, isChampion = false, isExtra = false, 0, Nil, Nil, PlayerTeam.WhiteBottom)

}
