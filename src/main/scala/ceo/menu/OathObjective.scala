package ceo.menu

sealed trait OathObjective {
  def involvesPlayingGames: Boolean = true
}

object OathObjective {

  case object EditArmyTesting extends OathObjective

  case object CompleteDailyChallenge extends OathObjective

  case object PlayCasualGames extends OathObjective

  case object PlayRankedGames extends OathObjective

  case object PlayCurrentGame extends OathObjective

}
