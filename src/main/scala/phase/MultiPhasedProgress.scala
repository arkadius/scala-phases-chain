package phase

class MultiPhasedProgress[-In, +Out](phasesChain: PhasesChain[In, Out]) {

  private var progressState = ProgressState(phasesChain.phasesCount)
  notifyAboutStatus()

  def run(in: In): Out = {
    val result = phasesChain.run(this)(in)
    finish(result)
    result
  }

  private[phase] def moveProgress(phasesCount: Int) {
    (1 to phasesCount).foreach(_ => endPhase())
  }

  private[phase] def inPhase[T](phaseName: String)(action: => T): T = {
    beginPhase(phaseName)
    val result = action
    endPhase()
    result
  }
  
  private def beginPhase(phaseName: String) {
    progressState = progressState.beginPhase(phaseName)
    notifyAboutStatus()
  }

  private def endPhase() {
    progressState = progressState.endPhase
    notifyAboutStatus()
  }

  protected def finish(result: Any) {
    progressState = progressState.finish(result)
    notifyAboutStatus()
  }

  private def notifyAboutStatus() {
    println(s"PROGRESS: ${progressState.status}")
  }

}