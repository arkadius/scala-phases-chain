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

import scalaz._
import Scalaz._

private[phase] trait ValidatingPhasesChain[-In, +OutF, +OutS] extends PhasesChain[In, Validation[OutF, OutS]] {

  def ::[NIn, NOutF >: OutF](prev: ValidatingPhasesChain[NIn, NOutF, In]): ValidatingPhasesChain[NIn, NOutF, OutS] =
    new ChainedValidatingPhasesChain(prev, this)

  protected def processIfSuccessOrMoveProgress[MidF, MidS, OutSS](progress: MultiPhasedProgress)
                                                                 (next: ValidatingPhasesChain[MidS, MidF, OutSS])
                                                                 (mid: Validation[MidF, MidS]) = mid match {
    case Success(s) =>
      next.processWithProgress(progress)(s)
    case Failure(f) =>
      next.moveProgress(progress)
      f.failure
  }

  protected def moveProgress(progress: MultiPhasedProgress) {
    progress.skipPhases(phasesDetails)
  }
}

private[phase] class ChainedValidatingPhasesChain[-In, MidF, MidS, +OutS](prev: ValidatingPhasesChain[In, MidF, MidS],
                                                                          next: ValidatingPhasesChain[MidS, MidF, OutS])
  extends ValidatingPhasesChain[In, MidF, OutS] {

  def processWithProgress(progress: MultiPhasedProgress) = {
    val processPrev = prev.processWithProgress(progress)
    processPrev andThen processIfSuccessOrMoveProgress(progress)(next)
  }

  val phasesDetails = prev.phasesDetails ::: next.phasesDetails

}

private[phase] case class EmptyValidatingChain[S]() extends ValidatingPhasesChain[S, Nothing, S] {
  def processWithProgress(progress: MultiPhasedProgress): S => Validation[Nothing, S] = in => in.success

  val phasesDetails = Nil
}

trait SingleValidatingPhaseChain[-In, +OutF, +OutS] extends ValidatingPhasesChain[In, OutF, OutS] {
  val name: String
  def process(in: In): Validation[OutF, OutS]
  private lazy val details = PhaseDetails(name)

  private[phase] def processWithProgress(progress: MultiPhasedProgress): In => Validation[OutF, OutS] = processWrapped(details, process)(progress)

  def phasesDetails = List(details)
}

object ValidatingPhasesChain {
  def empty[S]: ValidatingPhasesChain[S, Nothing, S] = EmptyValidatingChain()
  
  def apply[In, OutF, OutS](n: String)
                           (proc: In => Validation[OutF, OutS]): ValidatingPhasesChain[In, OutF, OutS] =
    new SingleValidatingPhaseChain[In, OutF, OutS] {
      val name: String = n
      def process(in: In): Validation[OutF, OutS] = proc(in)
    }
}