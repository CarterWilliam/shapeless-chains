package carter.fchains.execution

import carter.fchains._
import carter.fchains.execution.Timing.Timed
import cats._
import cats.data.WriterT

object TimedExecutor extends ChainExecutor[Timed] {

  override def execute[Out](chain: Chain[Out]): Timed[Out] = chain match {
    case ChainRoot(provider) =>
      executeProvider(provider)
    case ChainStep(init, step) =>
      execute(init).flatMap(executeStep(step))
    case split: ChainSplit[_, Out] =>
      split.run(this)
  }

  def executeProvider[O](provider: Provider[O]): Timed[O] = Timing.timed {
    provider.f()
  }

  def executeStep[I, O](step: Transform[I, O])(i: I): Timed[O] = Timing.timed {
    step.f(i)
  }

}

object Timing {
  type Timed[T] = WriterT[Id, ExecutionTimes, T]

  implicit def timedMonad: Monad[Timed] = new Monad[Timed] {
    override def pure[A](x: A): Timed[A] = WriterT.put(x)(ExecutionTimes.Empty)
    override def flatMap[A, B](fa: Timed[A])(f: A => Timed[B]): Timed[B] =
      implicitly[FlatMap[Timed]].flatMap(fa)(f)
    override def tailRecM[A, B](a: A)(f: A => Timed[Either[A, B]]): Timed[B] =
      implicitly[FlatMap[Timed]].tailRecM(a)(f)
  }

  case class ExecutionTimes(times: Seq[ExecutionTime])
  object ExecutionTimes {
    def apply(time: Long): ExecutionTimes = ExecutionTimes(Seq(ExecutionTime(time)))

    val Empty: ExecutionTimes = ExecutionTimes(Nil)

    implicit val executionTimesSemiGroup: Semigroup[ExecutionTimes] = new Semigroup[ExecutionTimes] {
      override def combine(x: ExecutionTimes, y: ExecutionTimes): ExecutionTimes = ExecutionTimes(x.times ++ y.times)
    }
  }
  case class ExecutionTime(nanos: Long)

  def timed[O](f: => O): Timed[O] = {
    val startNanos: Long = System.nanoTime()
    val out: O = f
    val endNanos: Long = System.nanoTime()

    WriterT.put(out)(ExecutionTimes(endNanos - startNanos))
  }
}
