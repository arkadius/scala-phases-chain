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

private[phase] sealed abstract class ProgressState {
  import ProgressState._

  def start(): ProgressState
  def init(totalPhases: Seq[PhaseDetails]): ProgressState
  def beginPhase(details: PhaseDetails): ProgressState
  def endPhase(details: PhaseDetails): ProgressState
  def finish(result: Any): ProgressState
  def fail(ex: Throwable): ProgressState = FailedState(ex)
  def status: OperationStatus
}

private[phase] object ProgressState {
  import OperationProgress._

  val additionalPreparingPhase = PhaseDetails("Preparing")

  def apply(): ProgressState = NotStartedState

  private case object NotStartedState extends ProgressState {
    def start() = NotInitializedState
    def init(totalPhases: Seq[PhaseDetails]) = throw new IllegalStateException("Progress is not started")
    def beginPhase(details: PhaseDetails) = throw new IllegalStateException("Progress is not started")
    def endPhase(details: PhaseDetails) = throw new IllegalStateException("Progress is not started")
    def finish(result: Any) = throw new IllegalStateException("Progress is not started")
    def status = NotStarted
  }

  private case object NotInitializedState extends ProgressState {
    def start() = throw new IllegalStateException("Progress is already started")
    def init(totalPhases: Seq[PhaseDetails]) = InitState(totalPhases.toSet)
    def beginPhase(details: PhaseDetails) = throw new IllegalStateException("Progress is not initialized")
    def endPhase(details: PhaseDetails) = throw new IllegalStateException("Progress is not initialized")
    def finish(result: Any) = throw new IllegalStateException("Progress is not initialized")
    def status = InProgress(additionalPreparingPhase.name, MIN_PROGRESS)
  }

  private case class InitState(private val totalPhases: Set[PhaseDetails]) extends ProgressState {
    private val totalPhasesWithPreparing: Set[PhaseDetails] = totalPhases + additionalPreparingPhase
    def start() = throw new IllegalStateException("Progress is already started")
    def init(totalPhases: Seq[PhaseDetails]) = throw new IllegalStateException("Progress is already initialized")
    def beginPhase(details: PhaseDetails): ProgressState = InProgressState(totalPhasesWithPreparing, details).endPhase(additionalPreparingPhase)
    def endPhase(details: PhaseDetails) = throw new IllegalStateException("Progress is not in progress")
    def finish(result: Any): ProgressState = FinalState(result)
    def status = InProgress("Preparing", MIN_PROGRESS)
    override def toString: String = s"${getClass.getSimpleName}(${totalPhasesWithPreparing.size})"
  }

  private case class InProgressState(private val totalPhases: Set[PhaseDetails], private val currentPhase: PhaseDetails, private val finishedPhases: Set[PhaseDetails] = Set()) extends ProgressState {
    def start() = throw new IllegalStateException("Progress is already started")
    def init(totalPhases: Seq[PhaseDetails]) = throw new IllegalStateException("Progress is already initialized")
    def beginPhase(details: PhaseDetails) = copy(currentPhase = details)
    def endPhase(details: PhaseDetails) = copy(finishedPhases = finishedPhases + details)
    def finish(result: Any) = FinalState(result)
    def status = {
      val progress = MIN_PROGRESS + (MAX_PROGRESS - MIN_PROGRESS) * finishedPhases.size / totalPhases.size
      InProgress(currentPhase.name, progress)
    }
    override def toString: String = s"${getClass.getSimpleName}('${currentPhase.name}' [${finishedPhases.size} / ${totalPhases.size}])"
  }

  private case class FinalState(private val result: Any) extends ProgressState {
    def start() = throw new IllegalStateException("Progress wasn't reset")
    def init(totalPhases: Seq[PhaseDetails]) = throw new IllegalStateException("Progress is already initialized")
    def beginPhase(details: PhaseDetails) = throw new IllegalStateException("Progress is already finished")
    def endPhase(details: PhaseDetails) = throw new IllegalStateException("Progress is already finished")
    def finish(result: Any) = throw new IllegalStateException("Progress is already finished")
    def status = Completed(result)
    override def toString: String = s"${getClass.getSimpleName}"
  }

  private case class FailedState(private val ex: Throwable) extends ProgressState {
    def start() = throw new IllegalStateException("Progress wasn't reset")
    def init(totalPhases: Seq[PhaseDetails]) = throw new IllegalStateException("Progress is already finished")
    def beginPhase(details: PhaseDetails) = throw new IllegalStateException("Progress is already finished")
    def endPhase(details: PhaseDetails) = throw new IllegalStateException("Progress is already finished")
    def finish(result: Any) = throw new IllegalStateException("Progress is already finished")
    def status = Failed(ex)
    override def toString: String = s"${getClass.getSimpleName}(${ex.getMessage})"
  }

}