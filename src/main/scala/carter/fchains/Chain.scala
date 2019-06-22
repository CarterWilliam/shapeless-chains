package carter.fchains

import shapeless._
import shapeless.ops.hlist.{ConstMapper, Mapper, ZipApply}

case class Provider[Out](f: () => Out)
case class Transform[In, Out](f: In => Out)
case class Merge[In <: HList, Out](f: In => Out)

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

  case class SplitChain[Rep <: HList](chains: Rep)

  // Given a Transform[I, O], create a Function[Transform[I, O], Chain[O]]
  object ChainableFunction extends Poly1 {
    implicit def atTransform[I, O] = at[Transform[I, O]] { transform: Transform[I, O] =>
      (chain: Chain[I]) => ChainStep(chain, transform)
    }
  }

  implicit class ChainSyntax[I](chain: Chain[I]) {
    type TransformI[O] = Transform[I, O]

    def ~~>[O](transform: Transform[I, O]): Chain[O] = ChainStep(chain, transform)

    def ~~<[TRep <: HList, FRep <: HList, CRep <: HList]
        (transforms: TRep)
        (implicit
         mapper: Mapper.Aux[ChainableFunction.type, TRep, FRep],
         mapConst: ConstMapper.Aux[Chain[I], TRep, CRep],
         zipApply: ZipApply[FRep, CRep]): SplitChain[zipApply.Out] = {
      SplitChain {
        zipApply(mapper(transforms), mapConst(chain, transforms))
      }
    }
  }
}
