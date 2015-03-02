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

import akka.actor.ActorRef

abstract class MultiPhasedProgress {

  private[phase] def skipPhases(phases: Seq[PhaseDetails]) {
    phases.foreach(skipPhase)
  }

  protected def skipPhase(phaseDetails: PhaseDetails) {
    beginPhase(phaseDetails)
    endPhase(phaseDetails)
  }

  private[phase] def inPhase[T](phaseDetails: PhaseDetails)(action: => T): T = {
    beginPhase(phaseDetails)
    val result = action
    endPhase(phaseDetails)
    result
  }

  protected def beginPhase(phaseDetails: PhaseDetails)

  protected def endPhase(phaseDetails: PhaseDetails)

  private[phase] def finish(result: Any)

}

private[phase] class NotifyingActorProgress(progressActor: ActorRef, phasesDetails: Seq[PhaseDetails])
  extends MultiPhasedProgress  {

  progressActor ! Init(phasesDetails)

  protected def beginPhase(phaseDetails: PhaseDetails) {
    progressActor ! BeginPhase(phaseDetails)
  }

  protected def endPhase(phaseDetails: PhaseDetails) {
    progressActor ! EndPhase(phaseDetails)
  }

  private[phase] def finish(result: Any) {
    progressActor ! Finish(result)
  }
}

class ChainRunner[-In, +Out](chain: PhasesChain[In, Out], progress: MultiPhasedProgress) {
  def run(in: In): Out = {
    val result = chain.run(progress)(in)
    progress.finish(result)
    result
  }
}

object ChainRunner {
  def actorBased[In, Out](chain: PhasesChain[In, Out], progressActor: ActorRef): ChainRunner[In, Out] = {
    val progress = new NotifyingActorProgress(progressActor, chain.phasesDetails)
    new ChainRunner(chain, progress)
  }
}