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

private[phase] trait PhasesChain[-In, +Out] extends ChainTransformation[In, Out] {

  private[phase] def run(progress: MultiPhasedProgress)(in: In): Out = {
    processWithProgress(progress)(in)
  }

  private[phase] def processWithProgress(progress: MultiPhasedProgress): In => Out

  def ::[NIn](prev: PhasesChain[NIn, In]): PhasesChain[NIn, Out] = new ChainedPhasesChain(prev, this)

  protected def processWrapped[I, O](details: PhaseDetails, process: I => O)
                                    (progress: MultiPhasedProgress) = (in: I) =>
    progress.inPhase(details) {
      process(in)
    }

  private[phase] def phasesDetails: List[PhaseDetails]
  def phasesCount = phasesDetails.size

}

private[phase] class ChainedPhasesChain[-In, Mid, +Out](prev: PhasesChain[In, Mid], next: PhasesChain[Mid, Out])
  extends PhasesChain[In, Out] {

  def processWithProgress(progress: MultiPhasedProgress) = {
    val processPrev = prev.processWithProgress(progress)
    val processNext = next.processWithProgress(progress)
    processPrev andThen processNext
  }

  val phasesDetails = prev.phasesDetails ::: next.phasesDetails

}

private[phase] case class EmptyChain[T]() extends PhasesChain[T, T] {
  def processWithProgress(progress: MultiPhasedProgress): T => T = in => in

  val phasesDetails = Nil
}

trait SinglePhaseChain[-In, +Out] extends PhasesChain[In, Out] {
  val name: String
  def process(in: In): Out
  private lazy val details = PhaseDetails(name)

  private[phase] def processWithProgress(progress: MultiPhasedProgress): In => Out = processWrapped(details, process)(progress)

  def phasesDetails = List(details)
}

object PhasesChain extends SplitterChainCreation {
  def empty[T]: PhasesChain[T, T] = EmptyChain()

  def apply[In, Out](n: String)
                    (proc: In => Out): PhasesChain[In, Out] = new SinglePhaseChain[In, Out] {
    val name: String = n
    def process(in: In): Out = proc(in)
  }
}

private[phase] case class PhaseDetails(name: String)