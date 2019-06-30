package carter.fchains

import carter.fchains.ChainDsl._
import org.specs2.mutable.Specification
import org.specs2.specification.Scope
import shapeless._

class ChainSpec extends Specification {

  "The Chain DSL" should {

    "chain transforms with chains" in new ChainScope {
      val chain = root ~~> strLen
      chain.get() must be equalTo ChainStep(root, strLen)
    }

    "chain multiple transforms with a chain" in new ChainScope {
      val transforms = strLen :: strRev :: HNil
      val chains = root ~~< transforms
      val expected = ChainSplit(ChainStep(root, strLen) :: ChainStep(root, strRev) :: HNil)
      TheChainExecutor.execute(chains.get()) must be equalTo 4 :: "evif" :: HNil
      chains.get() must be equalTo expected
    }

    "merge split chains with a merge" in new ChainScope {
      val merge = Transform[Int :: String :: HNil, String] {
        case len :: rev :: HNil => len + ":" + rev
      }
      val transforms = strLen :: strRev :: HNil
      val chain = root ~~< transforms >~~ merge
      chain.get() must be equalTo
        ChainStep(ChainSplit(ChainStep(root, strLen) :: ChainStep(root, strRev) :: HNil), merge)
    }
  }
}

trait ChainScope extends Scope {
  val root = ChainRoot(Provider(() => "five"))
  val strLen = Transform[String, Int](_.length)
  val strRev = Transform[String, String](_.reverse)
}