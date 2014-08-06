package phase

class MultiPhasedProgress(phasesCount: Int) {

  private var progressState = ProgressState(phasesCount)
  notifyAboutStatus()

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

  private[phase] def finish(result: Any) {
    progressState = progressState.finish(result)
    notifyAboutStatus()
  }

  private def notifyAboutStatus() {
    println(s"PROGRESS: ${progressState.status}")
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
  def apply[In, Out](chain: PhasesChain[In, Out]): ChainRunner[In, Out] = {
    val progress = new MultiPhasedProgress(chain.phasesCount)
    new ChainRunner(chain, progress)
  }
}