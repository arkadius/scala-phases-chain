/*
 * Copyright 2015 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package phase

import org.scalatest._

import scalaz.Scalaz._
import scalaz._

class PhasesChainTest extends fixture.FunSpec with Matchers {

  class FixtureParam {
    var progress: MultiPhasedProgressMock = _

    def apply[In, Out](chain: PhasesChain[In, Out], in: In): Out = {
      progress = new MultiPhasedProgressMock
      val runner = new ChainRunner(chain, progress)
      runner.run(in)
    }

    def phasesInOutsEquals(expected: List[String]) {
      progress.phasesIns  shouldEqual expected
      progress.phasesOuts shouldEqual expected
    }
  }

  protected def withFixture(test: OneArgTest): Outcome = test(new FixtureParam)

  describe("Simple Phase") {
    it ("should work single") { run =>
      var arg: Int = -1
      val single = SinglePhaseChain("single") {(i:Int) => arg = i; 1}
      single.phasesCount shouldBe 1

      val result = run(single, 0)

      arg shouldBe 0
      result shouldBe 1
      run.phasesInOutsEquals("single" :: Nil)
    }

    it ("should work for chain") { run =>
      var args = Map[String, Any]()
      val chained =
        SinglePhaseChain("p1") { (arg:Int) => args += "p1" -> arg; 1} ::
          SinglePhaseChain("p2") { (arg:Int) => args += "p2" -> arg; 2} ::
          SinglePhaseChain("p3") { (arg:Int) => args += "p3" -> arg; 3}
      chained.phasesCount shouldBe 3

      val result = run(chained, 0)

      args("p1") shouldBe 0
      args("p2") shouldBe 1
      args("p3") shouldBe 2
      result shouldBe 3
      run.phasesInOutsEquals("p1" :: "p2" :: "p3" :: Nil)
    }

    it ("should do nothing for nil phase") { run =>
      val empty = EmptyChain[Int]()
      empty.phasesCount shouldBe 0

      val result = run(empty, 0)

      result shouldBe 0
      run.phasesInOutsEquals(Nil)
    }
  }

  describe("Validating Phase") {
    it ("should work single") { run =>
      var arg: Int = -1
      val single = SingleValidatingPhaseChain("single") {(i:Int) => arg = i; 1.success }
      single.phasesCount shouldBe 1

      val result = run(single, 0)

      arg shouldBe 0
      result shouldBe 1.success
      run.phasesInOutsEquals("single" :: Nil)
    }

    it ("should work for chain") { run =>
      var args = Map[String, Any]()
      val chained =
        SingleValidatingPhaseChain("p1") { (arg:Int) => args += "p1" -> arg; 1.success} ::
          SingleValidatingPhaseChain("p2") { (arg:Int) => args += "p2" -> arg; 2.success} ::
          SingleValidatingPhaseChain("p3") { (arg:Int) => args += "p3" -> arg; 3.success}
      chained.phasesCount shouldBe 3

      val result = run(chained, 0)

      args("p1") shouldBe 0
      args("p2") shouldBe 1
      args("p3") shouldBe 2
      result shouldBe 3.success
      run.phasesInOutsEquals("p1" :: "p2" :: "p3" :: Nil)
    }

    it ("should break the chain if failure occurs") { run =>
      var args = Map[String, Any]()
      val chained =
        SingleValidatingPhaseChain("p1") { (arg:Int) => args += "p1" -> arg; 1.success} ::
          SingleValidatingPhaseChain("p2") { (arg:Int) => args += "p2" -> arg; "breaking".failure} ::
          SingleValidatingPhaseChain("p3") { (arg:Int) => args += "p3" -> arg; 3.success} ::
          SingleValidatingPhaseChain("p4") { (arg:Int) => args += "p4" -> arg; 4.success} ::
          SingleValidatingPhaseChain("p5") { (arg:Int) => args += "p5" -> arg; 5.success}
      chained.phasesCount shouldBe 5

      val result = run(chained, 0)

      args("p1") shouldBe 0
      args("p2") shouldBe 1
      args.get("p3") shouldBe empty
      args.get("p4") shouldBe empty
      args.get("p5") shouldBe empty
      result shouldBe "breaking".failure
      run.phasesInOutsEquals("p1" :: "p2" :: Nil)
      run.progress.movedProgresses shouldBe 3
    }

    it ("should do nothing for nil phase") { run =>
      val empty = EmptyValidatingChain[Int]()
      empty.phasesCount shouldBe 0

      val result = run(empty, 0)

      result shouldBe 0.success
      run.phasesInOutsEquals(Nil)
    }
  }

  describe("mixed phase and validating phase") {
    it ("should proper move") { run =>
      var args = Map[String, Any]()
      val chainedValidating =
        SingleValidatingPhaseChain("p1") { (arg:Int) => args += "p1" -> arg; 1.success} ::
          SingleValidatingPhaseChain("p2") { (arg:Int) => args += "p2" -> arg; "breaking".failure} ::
          SingleValidatingPhaseChain("p3") { (arg:Int) => args += "p3" -> arg; 3.success}
      val chainedAll =
        chainedValidating ::
          SinglePhaseChain("p4") { (arg: Validation[String, Int]) => args += "p4" -> arg; arg valueOr { _ => -1 }}
      chainedAll.phasesCount shouldBe 4

      val result = run(chainedAll, 0)

      args("p1") shouldBe 0
      args("p2") shouldBe 1
      args.get("p3") shouldBe empty
      args("p4") shouldBe "breaking".failure
      result shouldBe -1
      run.phasesInOutsEquals("p1" :: "p2" :: "p4" :: Nil)
      run.progress.movedProgresses shouldBe 1
    }
  }
}