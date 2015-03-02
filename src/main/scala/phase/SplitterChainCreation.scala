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

private[phase] trait SplitterChainCreation {

  def splitter[In, Out](seq: Seq[PhasesChain[In, Out]]): PhasesChain[In, Seq[Out]] = new SplitterChain(seq)

  def splitter[In, OutA, OutB](a: PhasesChain[In, OutA],
                               b: PhasesChain[In, OutB]): PhasesChain[In, (OutA, OutB)] =
    new SplitterChain2(a, b)

  def splitter[In, OutA, OutB, OutC](a: PhasesChain[In, OutA],
                                     b: PhasesChain[In, OutB],
                                     c: PhasesChain[In, OutC]): PhasesChain[In, (OutA, OutB, OutC)] =
    new SplitterChain3(a, b, c)


  private class SplitterChain[In, Out](seq: Seq[PhasesChain[In, Out]])
    extends PhasesChain[In, Seq[Out]] {

    private[phase] def processWithProgress(progress: MultiPhasedProgress): (In) => Seq[Out] = { in: In =>
      seq.map { phase =>
        phase.processWithProgress(progress)(in)
      }
    }

    val phasesDetails = seq.flatMap(_.phasesDetails).toList
  }

  private class SplitterChain2[In, OutA, OutB](a: PhasesChain[In, OutA], b: PhasesChain[In, OutB])
    extends PhasesChain[In, (OutA, OutB)] {

    private[phase] def processWithProgress(progress: MultiPhasedProgress): (In) => (OutA, OutB) = { in: In =>
      val aResult = a.processWithProgress(progress)(in)
      val bResult = b.processWithProgress(progress)(in)
      (aResult, bResult)
    }

    val phasesDetails = a.phasesDetails ::: b.phasesDetails
  }

  private class SplitterChain3[In, OutA, OutB, OutC](a: PhasesChain[In, OutA], b: PhasesChain[In, OutB], c: PhasesChain[In, OutC])
    extends PhasesChain[In, (OutA, OutB, OutC)] {

    private[phase] def processWithProgress(progress: MultiPhasedProgress): (In) => (OutA, OutB, OutC) = { in: In =>
      val aResult = a.processWithProgress(progress)(in)
      val bResult = b.processWithProgress(progress)(in)
      val cResult = c.processWithProgress(progress)(in)
      (aResult, bResult, cResult)
    }

    val phasesDetails = a.phasesDetails ::: b.phasesDetails ::: c.phasesDetails
  }

}
