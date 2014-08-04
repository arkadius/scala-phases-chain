package phase

class Phase[-In, +Out](private[phase] val name: String,
                       private[phase] val process: In => Out)

object Phase {
  def apply[In, Out](name: String)
                    (process: In => Out) = new Phase(name, process)
}

trait PhasesChain[-In, +Out] extends ChainTransformation[In, Out] {

  private[phase] def run(progress: MultiPhasedProgress[_, _])(in: In): Out = {
    processWithProgress(progress)(in)
  }

  private[phase] def processWithProgress(progress: MultiPhasedProgress[_, _]): In => Out

  def ::[NIn](prev: Phase[NIn, In]): PhasesChain[NIn, Out] = new ChainedPhase(prev, this)

  def ::[NIn](prev: PhasesChain[NIn, In]): PhasesChain[NIn, Out] = new ChainedPhasesChain(prev, this)

  protected def processWrapped[I, O](name: String, process: I => O)
                                    (progress: MultiPhasedProgress[_, _]) = (in: I) =>
    progress.inPhase(name) {
      process(in)
    }

  val phasesCount: Int

}

object PhasesChain extends SequencedChainCreation

private[phase] class ChainedPhase[-In, Mid, +Out](prev: Phase[In, Mid], next: PhasesChain[Mid, Out])
  extends PhasesChain[In, Out] {

  def processWithProgress(progress: MultiPhasedProgress[_, _]) = {
    val processPrevWrapped = processWrapped(prev.name, prev.process)(progress)
    val processNext = next.processWithProgress(progress)
    processPrevWrapped andThen processNext
  }

  val phasesCount = 1 + next.phasesCount

}

private[phase] class ChainedPhasesChain[-In, Mid, +Out](prev: PhasesChain[In, Mid], next: PhasesChain[Mid, Out])
  extends PhasesChain[In, Out] {

  def processWithProgress(progress: MultiPhasedProgress[_, _]) = {
    val processPrev = prev.processWithProgress(progress)
    val processNext = next.processWithProgress(progress)
    processPrev andThen processNext
  }

  val phasesCount = prev.phasesCount + next.phasesCount

}

object NilChain {
  def apply[T]() = new PhasesChain[T, T] {
    def processWithProgress(progress: MultiPhasedProgress[_, _]): (T) => T = in => in

    val phasesCount = 0
  }
}

object IdentityPhase {
  def apply[In](name: String)
               (process: In => Unit) = new Phase(name, { (in: In) =>
    process(in)
    in
  })
}