package ceo.play

import scala.collection.immutable.Seq

trait DynamicRunner[A, B] {

  def update(state: A, data: B): A

}

object DynamicRunner {
  def foldLeft[A, B](initialState: A, data: B, seq: Seq[DynamicRunner[A, B]]): A = {
    seq.foldLeft(initialState)((acc, runner) => runner.update(acc, data))
  }
}

sealed trait SimpleRunner[A] extends DynamicRunner[A, Unit] {

  override def update(state: A, data: Unit): A = update(state)

  def update(value: A): A

}
