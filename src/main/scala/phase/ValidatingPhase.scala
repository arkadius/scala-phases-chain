package phase

import scalaz.Scalaz._
import scalaz._

class ValidatingPhase[-In, +OutF, +OutS](_name: String, _process: In => Validation[OutF, OutS])
  extends Phase[In, Validation[OutF, OutS]](_name, _process)

object ValidatingPhase {
  def apply[In, OutF, OutS](name: String)
                           (process: In => Validation[OutF, OutS]) = new ValidatingPhase(name, process)
}

trait ValidatingPhasesChain[-In, +OutF, +OutS] extends PhasesChain[In, Validation[OutF, OutS]] {

  def ::[NIn, NOutF >: OutF](prev: ValidatingPhase[NIn, NOutF, In]): ValidatingPhasesChain[NIn, NOutF, OutS] =
    new ChainedValidatingPhase(prev, this)

  def ::[NIn, NOutF >: OutF](prev: ValidatingPhasesChain[NIn, NOutF, In]): ValidatingPhasesChain[NIn, NOutF, OutS] =
    new ChainedValidatingPhasesChain(prev, this)

  protected def processIfSuccessOrMoveProgress[MidF, MidS, OutSS](progress: MultiPhasedProgress[_, _])
                                                                 (next: ValidatingPhasesChain[MidS, MidF, OutSS])
                                                                 (mid: Validation[MidF, MidS]) = mid match {
    case Success(s) =>
      next.processWithProgress(progress)(s)
    case Failure(f) =>
      next.moveProgress(progress)
      f.failure
  }

  protected def moveProgress(progress: MultiPhasedProgress[_, _]) {
    progress.moveProgress(phasesCount)
  }

}

private[phase] class ChainedValidatingPhase[-In, MidF, MidS, +OutS](prev: ValidatingPhase[In, MidF, MidS],
                                                                    next: ValidatingPhasesChain[MidS, MidF, OutS])
  extends ValidatingPhasesChain[In, MidF, OutS] {

  def processWithProgress(progress: MultiPhasedProgress[_, _]) = {
    val processPrevWrapped = processWrapped(prev.name, prev.process)(progress)
    processPrevWrapped andThen processIfSuccessOrMoveProgress(progress)(next)
  }

  val phasesCount = 1 + next.phasesCount

}

private[phase] class ChainedValidatingPhasesChain[-In, MidF, MidS, +OutS](prev: ValidatingPhasesChain[In, MidF, MidS],
                                                                             next: ValidatingPhasesChain[MidS, MidF, OutS])
  extends ValidatingPhasesChain[In, MidF, OutS] {

  def processWithProgress(progress: MultiPhasedProgress[_, _]) = {
    val processPrev = prev.processWithProgress(progress)
    processPrev andThen processIfSuccessOrMoveProgress(progress)(next)
  }

  val phasesCount = prev.phasesCount + next.phasesCount

}

object NilValidatingChain {
  def apply[S]() = new ValidatingPhasesChain[S, Nothing, S] {
    def processWithProgress(progress: MultiPhasedProgress[_, _]): (S) => Validation[Nothing, S] = in => in.success

    val phasesCount = 0
  }
}