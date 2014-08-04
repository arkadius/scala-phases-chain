package phase

sealed abstract class ProgressState {
  def beginPhase(phaseName: String): ProgressState = throw new IllegalStateException("Progress is not inited")
  def endPhase: ProgressState = throw new IllegalStateException("Operation is not in progress")
  def finish(result: Any): ProgressState = throw new IllegalStateException("Operation is already finished")
  def status: OperationStatus
}

object ProgressState {

  def apply(phasesCount: Int): ProgressState = InitedState(phasesCount)

  private case class InitedState(phasesCount: Int) extends ProgressState {
    override def beginPhase(phaseName: String) = InProgressState(phasesCount, phaseName)
    override def finish(result: Any) = FinishedState(result)

    def status: OperationStatus = NotStarted
  }

  private case class InProgressState(phasesCount: Int, phaseName: String, currentPhase: Int = 0) extends ProgressState {
    import OperationProgress._

    override def beginPhase(phaseName: String) = copy(phaseName = phaseName)
    override def endPhase = copy(currentPhase = currentPhase + 1)
    override def finish(result: Any) = FinishedState(result)

    def status = {
      val progress = MIN_PROGRESS + (MAX_PROGRESS - MIN_PROGRESS) * currentPhase / phasesCount
      InProgress(phaseName, progress)
    }
  }

  private case class FinishedState(private val result: Any) extends ProgressState {
    def status = Completed(result)
  }

}