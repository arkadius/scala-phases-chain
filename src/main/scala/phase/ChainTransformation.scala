package phase

private[phase] trait ChainTransformation[-In, +Out] { self: PhasesChain[In, Out] =>

  def transformIn[NewIn](f: NewIn => In): PhasesChain[NewIn, Out] = new ChainWithTransformedIn(this, f)

  def transformOut[NewOut](f: Out => NewOut): PhasesChain[In, NewOut] = new ChainWithTransformedOut(this, f)

  def wrap[NewIn, NewOut](f: NewIn => (In => Out) => NewOut): PhasesChain[NewIn, NewOut] = new WrappingChain(this, f)

}

private[phase] class ChainWithTransformedIn[-NewIn, -In, +Out](chain: PhasesChain[In, Out], transformIn: NewIn => In) extends PhasesChain[NewIn, Out] {
  private[phase] def processWithProgress(progress: MultiPhasedProgress[_, _]): NewIn => Out = { newIn =>
    val in = transformIn(newIn)
    chain.processWithProgress(progress)(in)
  }

  val phasesCount: Int = chain.phasesCount
}

private[phase] class ChainWithTransformedOut[-In, +Out, +NewOut](chain: PhasesChain[In, Out], transformOut: Out => NewOut) extends PhasesChain[In, NewOut] {
  private[phase] def processWithProgress(progress: MultiPhasedProgress[_, _]): In => NewOut = { in =>
    val out = chain.processWithProgress(progress)(in)
    transformOut(out)
  }

  val phasesCount: Int = chain.phasesCount
}

private[phase] class WrappingChain[-NewIn, In, +Out, +NewOut](chain: PhasesChain[In, Out], wrap: NewIn => (In => Out) => NewOut) extends PhasesChain[NewIn, NewOut] {
  private[phase] def processWithProgress(progress: MultiPhasedProgress[_, _]): NewIn => NewOut = { in =>
    wrap(in)(chain.processWithProgress(progress))
  }

  val phasesCount: Int = chain.phasesCount
}