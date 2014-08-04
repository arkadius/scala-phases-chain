package phase

sealed trait OperationStatus {
  def finishedProcess: Boolean = false
}

case object NotStarted extends OperationStatus

case class InProgress(phase: String, progress: Int) extends OperationStatus

case class Completed(result: Any) extends OperationStatus {
  override def finishedProcess = true
}

case class Failed(ex: Exception) extends OperationStatus {
  override def finishedProcess = true
}

object OperationProgress {
  val MIN_PROGRESS = 0
  val MAX_PROGRESS = 100
}