package ceo.menu

sealed trait OathObjective

object OathObjective {

  case object EditArmyTesting extends OathObjective

  case object CompleteDailyChallenge extends OathObjective

  case object PlayCasualGames extends OathObjective

}