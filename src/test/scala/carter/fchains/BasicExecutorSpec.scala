package carter.fchains

import org.specs2.mutable.Specification
import shapeless._

class BasicExecutorSpec extends Specification {

  "The ChainExecutor" should {
    "execute a chain root" in {
      val source = ChainRoot(Provider(() => 4))
      BasicExecutor.execute(source) should be equalTo 4
    }

    "execute a chain step" in {
      val chain = ChainStep(ChainRoot(Provider(() => 4)), Transform[Int, Int] { _ * 3 })
      BasicExecutor.execute(chain) should be equalTo 12
    }

    "execute split chains" in {
      val source = ChainRoot(Provider(() => 4))
      val add2 = Transform[Int, Int](_ + 2)
      val double = Transform[Int, Int](_ * 2)
      val addChain: Chain[Int] = ChainStep(source, add2)
      val dubChain: Chain[Int] = ChainStep(source, double)
      val chain = ChainSplit(addChain :: dubChain :: HNil)
      val expected = 6 :: 8 :: HNil
      BasicExecutor.execute(chain) should be equalTo expected
    }

    "execute a merge" in {
      val source = ChainRoot(Provider(() => 3))
      val add2 = Transform[Int, Int](_ + 2)
      val double = Transform[Int, Int](_ * 2)
      val addChain: Chain[Int] = ChainStep(source, add2)
      val dubChain: Chain[Int] = ChainStep(source, double)
      val splitChain = ChainSplit(addChain :: dubChain :: HNil)
      val mergeChain = ChainStep(splitChain, Transform[Int :: Int :: HNil, Int] { case one :: two :: HNil => one + two })
      BasicExecutor.execute(mergeChain) should be equalTo 11
    }
  }
}
