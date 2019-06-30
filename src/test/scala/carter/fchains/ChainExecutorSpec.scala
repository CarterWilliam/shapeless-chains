package carter.fchains

import org.specs2.mutable.Specification
import shapeless._

class ChainExecutorSpec extends Specification {
  import ChainDsl._

  "The ChainExecutor" should {
    "execute a chain root" in {
      val source = ChainRoot(Provider(() => 4))
      ChainExecutor(source) should be equalTo 4
    }

    "execute a chain step" in {
      val chain = ChainStep(ChainRoot(Provider(() => 4)), Transform[Int, Int] { _ * 3 })
      ChainExecutor(chain) should be equalTo 12
    }

    "execute split chains" in {
      val source = ChainRoot(Provider(() => 4))
      val add2 = Transform[Int, Int](_ + 2)
      val double = Transform[Int, Int](_ * 2)
      val addChain: Chainable[Int] = ChainStep(source, add2)
      val dubChain: Chainable[Int] = ChainStep(source, double)
      val chain = ChainSplit(addChain :: dubChain :: HNil)
      val expected = 6 :: 8 :: HNil
      ChainExecutor(chain) should be equalTo expected
    }

    "execute a merge" in {
      val source = ChainRoot(Provider(() => 3))
      val add2 = Transform[Int, Int](_ + 2)
      val double = Transform[Int, Int](_ * 2)
      val addChain: Chainable[Int] = ChainStep(source, add2)
      val dubChain: Chainable[Int] = ChainStep(source, double)
      val splitChain = ChainSplit(addChain :: dubChain :: HNil)
      val mergeChain = ChainStep(splitChain, Transform[Int :: Int :: HNil, Int] { case one :: two :: HNil => one + two })
      ChainExecutor(mergeChain) should be equalTo 11
    }
  }
}
