package ceo.play

import scala.util.Random

object ArmyGenerator {

  case class Army(minions: List[String], champions: List[String]) {
    def withColor(playerColor: PlayerColor): Army =
      copy(champions = champions.map(_ + "_" + playerColor), minions = minions.map(_ + "_" + playerColor))
  }

  def generateRandomArmy(piecesCanChangeStatusEffects: Boolean = true)(random: Random): Army = {
    val allPieces = DataLoader.getAllPieceData.filter { pieceData =>
      piecesCanChangeStatusEffects || !pieceData.canChangeStatusEffects
    }

    val allMinions = allPieces.filter(piece => piece.isMinion && !piece.isExtra).toArray
    val allChampions = allPieces.filter(piece => piece.isChampion && !piece.isExtra).toArray

    val minions = (1 to 8).map(_ => allMinions(random.nextInt(allMinions.length)).officialName)
    val championsWithoutKing = (1 to 7).map(_ => allChampions(random.nextInt(allChampions.length)).officialName)

    val champions = random.shuffle("King" +: championsWithoutKing)

    Army(minions.toList, champions.toList)
  }

  def generateStartingState(whiteArmy: Army, blackArmy: Army, isWhitePlayerAtBottom: Boolean): GameState = {
    val whiteArmyColor = whiteArmy.withColor(PlayerColor.White)
    val blackArmyColor = blackArmy.withColor(PlayerColor.Black)
    val lines =
      if (isWhitePlayerAtBottom) {
        List(
          blackArmyColor.champions.reverse.mkString(" "),
          blackArmyColor.minions.reverse.mkString(" "),
          "", "", "", "",
          whiteArmyColor.minions.mkString(" "),
          whiteArmyColor.champions.mkString(" ")
        )
      } else {
        List(
          whiteArmyColor.champions.reverse.mkString(" "),
          whiteArmyColor.minions.reverse.mkString(" "),
          "", "", "", "",
          blackArmyColor.minions.mkString(" "),
          blackArmyColor.champions.mkString(" ")
        )
      }
    DataLoader.loadBoard(lines, isWhitePlayerAtBottom)
  }

}
