package phase

import org.scalatest._

import scalaz.Scalaz._
import scalaz._

class PhaseTest extends FunSpec with Matchers {

  describe("Simple Phase") {
    it ("should work single") {
      var arg: Int = -1
      val single = Phase("single") {(i:Int) => arg = i; 1} :: NilChain()
      single.phasesCount shouldBe 1

      implicit val progress = new MultiPhasedProgressMock(single)
      val result = progress.run(0)

      arg shouldBe 0
      result shouldBe 1
      phasesInOutsEquals("single" :: Nil)
    }

    it ("should work for chain") {
      var args = Map[String, Any]()
      val chained =
        Phase("p1") { (arg:Int) => args += "p1" -> arg; 1} ::
          Phase("p2") { (arg:Int) => args += "p2" -> arg; 2} ::
          Phase("p3") { (arg:Int) => args += "p3" -> arg; 3} :: NilChain[Int]()
      chained.phasesCount shouldBe 3

      implicit val progress = new MultiPhasedProgressMock(chained)
      val result = progress.run(0)

      args("p1") shouldBe 0
      args("p2") shouldBe 1
      args("p3") shouldBe 2
      result shouldBe 3
      phasesInOutsEquals("p1" :: "p2" :: "p3" :: Nil)
    }

    it ("should do nothing for nil phase") {
      val nil = NilChain[Int]()
      nil.phasesCount shouldBe 0

      implicit val progress = new MultiPhasedProgressMock(nil)
      val result = progress.run(0)

      result shouldBe 0
      phasesInOutsEquals(Nil)
    }
  }

  describe("Validating Phase") {
    it ("should work single") {
      var arg: Int = -1
      val single = ValidatingPhase("single") {(i:Int) => arg = i; 1.success } :: NilValidatingChain()
      single.phasesCount shouldBe 1

      implicit val progress = new MultiPhasedProgressMock(single)
      val result = progress.run(0)

      arg shouldBe 0
      result shouldBe 1.success
      phasesInOutsEquals("single" :: Nil)
    }

    it ("should work for chain") {
      var args = Map[String, Any]()
      val chained =
        ValidatingPhase("p1") { (arg:Int) => args += "p1" -> arg; 1.success} ::
          ValidatingPhase("p2") { (arg:Int) => args += "p2" -> arg; 2.success} ::
          ValidatingPhase("p3") { (arg:Int) => args += "p3" -> arg; 3.success} :: NilValidatingChain()
      chained.phasesCount shouldBe 3

      implicit val progress = new MultiPhasedProgressMock(chained)
      val result = progress.run(0)

      args("p1") shouldBe 0
      args("p2") shouldBe 1
      args("p3") shouldBe 2
      result shouldBe 3.success
      phasesInOutsEquals("p1" :: "p2" :: "p3" :: Nil)
    }

    it ("should break the chain if failure occurs") {
      var args = Map[String, Any]()
      val chained =
        ValidatingPhase("p1") { (arg:Int) => args += "p1" -> arg; 1.success} ::
          ValidatingPhase("p2") { (arg:Int) => args += "p2" -> arg; "breaking".failure} ::
          ValidatingPhase("p3") { (arg:Int) => args += "p3" -> arg; 3.success} ::
          ValidatingPhase("p4") { (arg:Int) => args += "p4" -> arg; 4.success} ::
          ValidatingPhase("p5") { (arg:Int) => args += "p5" -> arg; 5.success} :: NilValidatingChain()
      chained.phasesCount shouldBe 5

      implicit val progress = new MultiPhasedProgressMock(chained)
      val result = progress.run(0)

      args("p1") shouldBe 0
      args("p2") shouldBe 1
      args.get("p3") shouldBe empty
      args.get("p4") shouldBe empty
      args.get("p5") shouldBe empty
      result shouldBe "breaking".failure
      phasesInOutsEquals("p1" :: "p2" :: Nil)
      progress.movedProgresses shouldBe 3
    }

    it ("should do nothing for nil phase") {
      val nil = NilValidatingChain[Int]()
      nil.phasesCount shouldBe 0

      implicit val progress = new MultiPhasedProgressMock(nil)
      val result = progress.run(0)

      result shouldBe 0.success
      phasesInOutsEquals(Nil)
    }
  }

  describe("mixed phase and validating phase") {
    it ("should work for chain") {
      var args = Map[String, Any]()
      val nested = Phase("p2") { (arg:Int) => args += "p2" -> arg; 2} :: NilChain[Int]()
      val chained =
        Phase("p1") { (arg:Int) => args += "p1" -> arg; 1} ::
          nested ::
          Phase("p3") { (arg:Int) => args += "p3" -> arg; 3} :: NilChain[Int]()
      chained.phasesCount shouldBe 3

      implicit val progress = new MultiPhasedProgressMock(chained)
      val result = progress.run(0)

      args("p1") shouldBe 0
      args("p2") shouldBe 1
      args("p3") shouldBe 2
      result shouldBe 3
      phasesInOutsEquals("p1" :: "p2" :: "p3" :: Nil)
    }

    it ("should proper move") {
      var args = Map[String, Any]()
      val chainedValidating =
        ValidatingPhase("p1") { (arg:Int) => args += "p1" -> arg; 1.success} ::
          ValidatingPhase("p2") { (arg:Int) => args += "p2" -> arg; "breaking".failure} ::
          ValidatingPhase("p3") { (arg:Int) => args += "p3" -> arg; 3.success} :: NilValidatingChain()
      val chainedAll =
        chainedValidating ::
          Phase("p4") { (arg: Validation[String, Int]) => args += "p4" -> arg; arg valueOr { _ => -1 }} :: NilChain()
      chainedAll.phasesCount shouldBe 4

      implicit val progress = new MultiPhasedProgressMock(chainedAll)
      val result = progress.run(0)

      args("p1") shouldBe 0
      args("p2") shouldBe 1
      args.get("p3") shouldBe empty
      args("p4") shouldBe "breaking".failure
      result shouldBe -1
      phasesInOutsEquals("p1" :: "p2" :: "p4" :: Nil)
      progress.movedProgresses shouldBe 1
    }

    it ("should proper nest validation phases") {
      var args = Map[String, Any]()
      val chainedValidating =
        ValidatingPhase("p1") { (arg:Int) => args += "p1" -> arg; 1.success} ::
          ValidatingPhase("p2") { (arg:Int) => args += "p2" -> arg; "breaking".failure} ::
          ValidatingPhase("p3") { (arg:Int) => args += "p3" -> arg; 3.success} :: NilValidatingChain()
      val chainedAll =
        chainedValidating ::
          ValidatingPhase("p4") { (arg: Int) => args += "p4" -> arg; 4.success} :: NilValidatingChain()
      chainedAll.phasesCount shouldBe 4

      implicit val progress = new MultiPhasedProgressMock(chainedAll)
      val result = progress.run(0)

      args("p1") shouldBe 0
      args("p2") shouldBe 1
      args.get("p3") shouldBe empty
      args.get("p4") shouldBe empty
      result shouldBe "breaking".failure
      phasesInOutsEquals("p1" :: "p2" :: Nil)
      progress.movedProgresses shouldBe 2
    }
  }

  def phasesInOutsEquals(expected: List[String])(implicit progress: MultiPhasedProgressMock[_, _]) {
    progress.phasesIns  shouldEqual expected
    progress.phasesOuts shouldEqual expected
  }

}