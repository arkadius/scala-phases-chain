package phase

import scalaz.Validation

object Implicits {

  implicit class ConvertibleToValidatingChain[-In, +OutF, +OutS](chain: PhasesChain[In, Validation[OutF, OutS]]) {
    def toValidating = new ValidatingPhasesChain[In, OutF, OutS] {
      private[phase] def processWithProgress(progress: MultiPhasedProgress): (In) => Validation[OutF, OutS] = chain.processWithProgress(progress)

      val phasesCount: Int = chain.phasesCount
    }
  }

}
