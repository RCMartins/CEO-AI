package ceo.play

trait DynamicRunner[A, B] {

  def update(value: A, data: B): A

}

object DynamicRunner {
  def foldLeft[A, B](initial: A, data: B, seq: Seq[DynamicRunner[A, B]]): A = {
    seq.foldLeft(initial)((acc, runner) => runner.update(acc, data))
  }
}

trait SimpleRunner[A] extends DynamicRunner[A, Unit] {

  override def update(value: A, data: Unit): A = update(value)

  def update(value: A): A

}

case class GameRunner(
  pieceMeleeDeathRunners: List[DynamicRunner[GameState, Piece]]
)

object GameRunner {
  val empty = GameRunner(Nil)
}
