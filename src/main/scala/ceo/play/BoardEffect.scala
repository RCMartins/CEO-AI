package ceo.play

sealed trait BoardEffect {
  def getReplayInfo: String
}

object BoardEffect {

  case class Lightning(boardPos: BoardPos, turnOfLightning: Double) extends BoardEffect {
    override def getReplayInfo: String = s"Lightning$boardPos $turnOfLightning"
  }

  case class Meteor(boardPos: BoardPos, turnOfMeteor: Double) extends BoardEffect {
    override def getReplayInfo: String = s"Meteor$boardPos $turnOfMeteor"
  }

  case class Butterfly(boardPos: BoardPos, turnOfEffect: Double, enchantDuration: Int, pieceDataOnRevive: PieceData) extends BoardEffect {
    override def getReplayInfo: String = s"Butterfly$boardPos $turnOfEffect $enchantDuration ${pieceDataOnRevive.nameWithPlayerBase}"
  }

}
