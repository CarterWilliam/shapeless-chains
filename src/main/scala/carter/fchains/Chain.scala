package carter.fchains

import carter.fchains.ChainDsl.Chainable
import shapeless._

case class Provider[Out](f: () => Out)
case class Transform[In, Out](f: In => Out)
case class Merge[In <: HList, Out](f: In => Out)

trait Chain[Out]
case class ChainRoot[Out](provider: Provider[Out]) extends Chain[Out]
case class ChainStep[In, Out](chain: Chain[In], transform: Transform[In, Out]) extends Chain[Out]
case class ChainSplit[+CH <: HList, OH <: HList](chains: CH)
     (implicit runner: SplitChainRunner.Aux[CH, OH]) extends Chain[OH] {

  def run(executor: ChainExecutor): OH = {
    runner.run(chains, executor)
  }
}


trait ChainExecutor {
  def execute[O](chain: Chain[O]): O
}

object RunChainable extends Poly1 {
  implicit def atChainable[O] = at[Chainable[O]](_.get())
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

  trait Mergable[CH <: HList, OH <: HList] {
    def get(): ChainSplit[CH, OH]
  }

//  // Given a Transform[I, O], create a Function[Transform[I, O], Chain[O]]
//  object ChainableFunction extends Poly1 {
//    implicit def atTransform[I, O] = at[Transform[I, O]] { transform: Transform[I, O] =>
//      (chain: Chainable[I]) => new Chainable[O] {
//        override def get(): Chain[O] = ChainStep(chain.get(), transform)
//      }
//    }
//  }

  implicit class ChainSyntax[I](chainable: Chainable[I]) {

    def ~~>[O](transform: Transform[I, O]): Chainable[O] = new Chainable[O] {
      override def get(): Chain[O] = ChainStep(chainable.get(), transform)
    }

    def ~~<[TRep <: HList, CRep <: HList, ORep <: HList]
        (transforms: TRep)
        (implicit
         splitter: ChainSplitter.Aux[I, TRep, CRep],
         runner: SplitChainRunner[CRep]): Mergable[CRep, runner.Out] = {

      new Mergable[CRep, runner.Out] {
        override def get(): ChainSplit[CRep, runner.Out] = {
          ChainSplit(splitter.split(transforms, chainable.get()))(runner)
        }
      }
    }
  }

  implicit class CSyntax[O](chain: Chain[O]) extends ChainSyntax[O](chain)

  implicit class MergableSyntax[CH <: HList, OH <: HList](mergable: Mergable[CH, OH]) {
    def >~~[O](merge: Transform[OH, O]): Chainable[O] = new Chainable[O] {
      override def get(): Chain[O] = ChainStep(mergable.get(), merge)
    }
  }

}

trait ChainSplitter[T, -TH <: HList] {
  type Out <: HList
  def split(transforms: TH, chain: Chain[T]): Out
}

object ChainSplitter {
  def apply[T, TH <: HList, CH <: HList](implicit splitter: ChainSplitter[T, TH]): Aux[T, TH, splitter.Out] = splitter

  type Aux[T, TH <: HList, CH <: HList] = ChainSplitter[T, TH] { type Out = CH }

  implicit def hnilChainSplitter[T]: Aux[T, HNil, HNil] = new ChainSplitter[T, HNil] {
    type Out = HNil
    def split(transforms: HNil, chain: Chain[T]): Out = HNil
  }

  implicit def hconsChainSplitter[T, O, TH <: HList, CH <: HList]
      (implicit splitter: Aux[T, TH, CH]): Aux[T, Transform[T, O] :: TH, Chain[O] :: CH] = {
    new ChainSplitter[T, Transform[T, O] :: TH] {
      type Out = Chain[O] :: CH
      def split(transforms: Transform[T, O] :: TH, chain: Chain[T]): Out = {
        ChainStep(chain, transforms.head) :: splitter.split(transforms.tail, chain)
      }
    }
  }
}

trait SplitChainRunner[-CH <: HList] {
  type Out <: HList
  def run(chains: CH, executor: ChainExecutor): Out
}

object SplitChainRunner {

  def apply[CH <: HList, OH <: HList]
    (implicit runner: SplitChainRunner[CH]): Aux[CH, runner.Out] = runner

  type Aux[CH <: HList, Out0 <: HList] = SplitChainRunner[CH] { type Out = Out0 }

  implicit def hnilRunner: Aux[HNil, HNil] = {
    new SplitChainRunner[HNil] {
      override type Out = HNil
      override def run(chains: HNil, executor: ChainExecutor): Out = HNil
    }
  }


  implicit def hconsRunner[O, CH <: HList, OH <: HList]
    (implicit runner: SplitChainRunner.Aux[CH, OH]): Aux[Chain[O] :: CH, O :: OH] = {
    new SplitChainRunner[Chain[O] :: CH] {
      override type Out = O :: OH
      override def run(chains: Chain[O] :: CH, executor: ChainExecutor): Out = {
        executor.execute(chains.head) :: runner.run(chains.tail, executor)
      }
    }
  }
}
