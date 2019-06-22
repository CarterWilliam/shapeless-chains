package carter.fchains

import shapeless._
import shapeless.poly._
import shapeless.ops.hlist.Mapper

case class Provider[Out](f: () => Out)
case class Transform[In, Out](f: In => Out)

sealed trait Chain[Out] {
  def run(): Out
}
case class ChainRoot[Out](provider: Provider[Out]) extends Chain[Out] {
  def run(): Out = provider.f.apply()
}
case class ChainStep[In, Out](chain: Chain[In], transform: Transform[In, Out]) extends Chain[Out] {
  def run(): Out = transform.f(chain.run())
}

object ChainDsl {

  implicit class ChainSyntax[I](chain: Chain[I]) {
    type TransformI[O] = Transform[I, O]

    def ~~>[O](transform: Transform[I, O]): Chain[O] = ChainStep(chain, transform)

    object StepUp extends (TransformI ~> Chain) {
      override def apply[O](transform: TransformI[O]): Chain[O] = ChainStep(chain, transform)
    }

    def ~~<[TRep <: HList, ORep <: HList]
        (transforms: TRep)
        (implicit
         constraint: LUBConstraint[TRep, Transform[I, _]],
         mapper: Mapper.Aux[StepUp.type, TRep, ORep]): mapper.Out = {
      transforms.map(StepUp)
    }

  }
}
