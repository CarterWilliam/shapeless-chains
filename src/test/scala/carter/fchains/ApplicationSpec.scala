package carter.fchains

import org.specs2.mutable.Specification

class ApplicationSpec extends Specification {

  "The Application" should {
    "convert strings to upper case" in {
      val application = new Application()
      application.toUpperCase("test") mustEqual "TEST"
    }
  }
}
