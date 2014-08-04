package phase

import org.scalatest.{Matchers, FlatSpec}

class MultiPhasedProgressTest extends FlatSpec with Matchers {

  it should "print progress state" in {
    val chain =
      Phase("Preparation") { in: Int => PreparedData(in) } ::
      Phase("Validation") { data: PreparedData => ValidatedData(data) } ::
      Phase("Execution") { data: ValidatedData => Result(data) } :: NilChain()

    val progress = new MultiPhasedProgress(chain)
    val result = progress.run(123)

    result shouldEqual Result(ValidatedData(PreparedData(123)))
  }

}

case class PreparedData(in: Int)
case class ValidatedData(prepared: PreparedData)
case class Result(validated: ValidatedData)