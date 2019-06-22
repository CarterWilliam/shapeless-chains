package carter.fchains

import org.specs2.mutable.Specification
import ChainDsl._
import org.specs2.specification.Scope
import shapeless.UnaryTCConstraint.*->*
import shapeless._

class ChainSpec extends Specification {

  "UnaryTCConstraint" should {

    "constrain an HList to Options" in {
      def acceptOption[L <: HList : *->*[Option]#λ](l : L) = true
      acceptOption(Option(23) :: Option(true) :: Option("foo") :: HNil) should beTrue
    }

//    "support type aliases?" in {
//      type StringFunction[O] = Function1[String, O]
//
//      def acceptStrFun[FL <: HList : *->*[StringFunction]#λ](functions: FL) = true
//
//      val reverse: String => String = _.reverse
//
////      optionsOnly(HNil : HNil)
////      optionsOnly(Option(5) :: HNil)
////      stringFunctionsOnly(HNil : HNil)
////      stringFunctionsOnly(fs)
//      acceptStrFun[HNil](HNil : HNil)
////      acceptStrFun(reverse :: HNil)
//    }

    "support string function TC" in {
      trait StringFunction[O] {
        def apply(s: String): O
      }
      object StringFunction {
        def apply[O](f: Function1[String, O]): StringFunction[O] = new StringFunction[O] {
          override def apply(s: String): O = f.apply(s)
        }
      }
      def acceptStrFun[FL <: HList : *->*[StringFunction]#λ](functions: FL) = true

      val strLen = StringFunction[Int](_.length)
      val strRev = StringFunction[String](_.reverse)

      acceptStrFun(strLen :: HNil)
      acceptStrFun(strLen :: strRev :: HNil)
    }

    "support Ifunction TC" in {
      trait TakesInput[I] {
        def apply[O](f: Function1[I, O]): O
      }
      object TakesInput {
        def apply[I](i: I): TakesInput[I] = new TakesInput[I] {
          override def apply[O](f: I => O): O = f(i)
        }
      }
      def acceptStrFun[FL <: HList : *->*[TakesInput]#λ](functions: FL) = true

      val strLen = TakesInput[String]("hmm")
      val strRev = TakesInput[String]("weird")

      acceptStrFun(strLen :: HNil)
      acceptStrFun(strLen :: strRev :: HNil)
    }

//    "support Ifunction lambda" in {
//      def acceptStrFun[FL <: HList](functions: FL)
//                                   (implicit unaryTCConstraint: UnaryTCConstraint[FL, ({ type F[O] = Function1[String, O] })#F]) = true
//
//      val len: String => Int = _.length
//      val reverse: String => String = _.reverse
//
//      acceptStrFun(HNil : HNil)
//      acceptStrFun(len :: reverse :: HNil)
//    }

  }

  "LUB Constraints" should {
    "only accept the right functions" in {
      def stringFunctionsOnly[FH <: HList](functions: FH)
                                          (implicit c: LUBConstraint[FH, Function[String, _]]) = true
      stringFunctionsOnly(HNil: HNil)
      val len: String => Int = _.length
      val reverse: String => String = _.reverse
      stringFunctionsOnly(len :: reverse :: HNil)
    }
  }

  "The Chain DSL" should {

    "chain transforms with chains" in new ChainScope {
      val chain = root ~~> strLen
      chain must be equalTo ChainStep(root, strLen)
    }

    "chain multiple transforms with a chain" in new ChainScope {
      val transforms = strLen :: strRev :: HNil
      val chains = root ~~< transforms
      val expected = SplitChain {
        ChainStep(root, strLen) :: ChainStep(root, strRev) :: HNil
      }
      chains must be equalTo expected
      chains.chains.head.run() should be equalTo 4
    }
  }
}

trait ChainScope extends Scope {
  val root = ChainRoot(Provider(() => "five"))
  val root5 = ChainRoot(Provider(() => 5))
  val strLen = Transform[String, Int](_.length)
  val strRev = Transform[String, String](_.reverse)
}