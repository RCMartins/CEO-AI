package ceo.menu

sealed trait OathObjective {
  def involvesPlayingGamesTimed: Boolean = false
}

object OathObjective {

  case object EditArmyTesting extends OathObjective

  case object CompleteDailyChallenge extends OathObjective

  case object PlayCasualGames extends OathObjective {
    override def involvesPlayingGamesTimed: Boolean = true
  }

  case object PlayRankedGames extends OathObjective {
    override def involvesPlayingGamesTimed: Boolean = true
  }

}
