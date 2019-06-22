package carter.fchains

import carter.fchains.ChainDsl.Chainable
import shapeless._
import shapeless.ops.hlist.{ConstMapper, Mapper, ZipApply}

case class Provider[Out](f: () => Out)
case class Transform[In, Out](f: In => Out)
case class Merge[In <: HList, Out](f: In => Out)

trait Chain[Out] {
  def run(): Out
}
case class ChainRoot[Out](provider: Provider[Out]) extends Chain[Out] {
  def run(): Out = provider.f.apply()
}
case class ChainStep[In, Out](chain: Chain[In], transform: Transform[In, Out]) extends Chain[Out] {
  def run(): Out = transform.f(chain.run())
}
case class ChainSplit[CH <: HList, OH <: HList](chains: CH)
     (implicit
      runAll: Mapper.Aux[RunChainable.type, CH, OH]) extends Chain[OH] {
  def run(): OH = runAll(chains)
}

object RunChainable extends Poly1 {
  implicit def atChainable[O] = at[Chainable[O]](_.get().run())
}

object ChainDsl {

  trait Chainable[T] {
    def get(): Chain[T]
  }
  object Chainable {
    implicit def fromChain[T](chain: Chain[T]): Chainable[T] = new Chainable[T] {
      override def get(): Chain[T] = chain
    }
  }

  // Given a Transform[I, O], create a Function[Transform[I, O], Chain[O]]
  object ChainableFunction extends Poly1 {
    implicit def atTransform[I, O] = at[Transform[I, O]] { transform: Transform[I, O] =>
      (chain: Chainable[I]) => new Chainable[O] {
        override def get(): Chain[O] = ChainStep(chain.get(), transform)
      }
    }
  }

  implicit class ChainSyntax[I](chainable: Chainable[I]) {
    type TransformI[O] = Transform[I, O]

    def ~~>[O](transform: Transform[I, O]): Chainable[O] = new Chainable[O] {
      override def get(): Chain[O] = ChainStep(chainable.get(), transform)
    }

    def ~~<[TRep <: HList, FRep <: HList, CRep <: HList, ORep <: HList, Out <: HList]
        (transforms: TRep)
        (implicit
         mapper: Mapper.Aux[ChainableFunction.type, TRep, FRep],
         mapConst: ConstMapper.Aux[Chainable[I], TRep, CRep],
         zipApply: ZipApply.Aux[FRep, CRep, ORep],
         outMapper: Mapper.Aux[RunChainable.type, ORep, Out]): Chainable[Out] = {

      new Chainable[Out] {
        override def get(): Chain[Out] = {
          ChainSplit(zipApply(mapper(transforms), mapConst(chainable, transforms)))
        }
      }
    }
  }

  implicit class CSyntax[O](chain: Chain[O]) extends ChainSyntax[O](chain)

}
