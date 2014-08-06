package phase

sealed abstract class ProgressState {
  def beginPhase(phaseName: String): ProgressState
  def endPhase: ProgressState
  def finish(result: Any): ProgressState
  def status: OperationStatus
}

object ProgressState {

  def apply(phasesCount: Int): ProgressState = InitState(phasesCount)

  private case class InitState(phasesCount: Int) extends ProgressState {
    def beginPhase(phaseName: String) = InProgressState(phasesCount, phaseName)
    def endPhase: ProgressState = throw new IllegalStateException("Operation is not in progress")
    def finish(result: Any) = FinalState(result)

    def status: OperationStatus = NotStarted
  }

  private case class InProgressState(phasesCount: Int, phaseName: String, currentPhase: Int = 0) extends ProgressState {
    import OperationProgress._

    def beginPhase(phaseName: String) = copy(phaseName = phaseName)
    def endPhase = copy(currentPhase = currentPhase + 1)
    def finish(result: Any) = FinalState(result)

    def status = {
      val progress = MIN_PROGRESS + (MAX_PROGRESS - MIN_PROGRESS) * currentPhase / phasesCount
      InProgress(phaseName, progress)
    }
  }

  private case class FinalState(private val result: Any) extends ProgressState {
    def beginPhase(phaseName: String): ProgressState = throw new IllegalStateException("Operation is already finished")
    def endPhase: ProgressState = throw new IllegalStateException("Operation is already finished")
    def finish(result: Any): ProgressState = throw new IllegalStateException("Operation is already finished")

    def status = Completed(result)
  }

}