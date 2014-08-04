package phase

private[phase] trait SequencedChainCreation {

  def sequenced[In, Out](seq: Seq[PhasesChain[In, Out]]): PhasesChain[In, Seq[Out]] = new SequencedChain(seq)

  def sequenced[In, OutA, OutB](a: PhasesChain[In, OutA],
                                b: PhasesChain[In, OutB]): PhasesChain[In, (OutA, OutB)] =
    new SequencedChain2(a, b)

  def sequenced[In, OutA, OutB, OutC](a: PhasesChain[In, OutA],
                                      b: PhasesChain[In, OutB],
                                      c: PhasesChain[In, OutC]): PhasesChain[In, (OutA, OutB, OutC)] =
    new SequencedChain3(a, b, c)

  private class SequencedChain[In, Out](seq: Seq[PhasesChain[In, Out]])
    extends PhasesChain[In, Seq[Out]] {

    private[phase] def processWithProgress(progress: MultiPhasedProgress[_, _]): (In) => Seq[Out] = { in: In =>
      seq.map { phase =>
        phase.processWithProgress(progress)(in)
      }
    }

    val phasesCount: Int = seq.size
  }

  private class SequencedChain2[In, OutA, OutB](a: PhasesChain[In, OutA], b: PhasesChain[In, OutB])
    extends PhasesChain[In, (OutA, OutB)] {

    private[phase] def processWithProgress(progress: MultiPhasedProgress[_, _]): (In) => (OutA, OutB) = { in: In =>
      val aResult = a.processWithProgress(progress)(in)
      val bResult = b.processWithProgress(progress)(in)
      (aResult, bResult)
    }

    val phasesCount: Int = a.phasesCount + b.phasesCount
  }

  private class SequencedChain3[In, OutA, OutB, OutC](a: PhasesChain[In, OutA], b: PhasesChain[In, OutB], c: PhasesChain[In, OutC])
    extends PhasesChain[In, (OutA, OutB, OutC)] {

    private[phase] def processWithProgress(progress: MultiPhasedProgress[_, _]): (In) => (OutA, OutB, OutC) = { in: In =>
      val aResult = a.processWithProgress(progress)(in)
      val bResult = b.processWithProgress(progress)(in)
      val cResult = c.processWithProgress(progress)(in)
      (aResult, bResult, cResult)
    }

    val phasesCount: Int = a.phasesCount + b.phasesCount + c.phasesCount
  }

}
