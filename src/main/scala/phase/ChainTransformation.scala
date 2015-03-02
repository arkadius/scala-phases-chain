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

private[phase] trait ChainTransformation[-In, +Out] { self: PhasesChain[In, Out] =>

  def transformInput[NewIn](f: NewIn => In): PhasesChain[NewIn, Out] = new ChainWithTransformedIn(this, f)

  def transformOutput[NewOut](f: Out => NewOut): PhasesChain[In, NewOut] = new ChainWithTransformedOut(this, f)

  def wrap[NewIn, NewOut](f: NewIn => (In => Out) => NewOut): PhasesChain[NewIn, NewOut] = new WrappingChain(this, f)

}

private[phase] class ChainWithTransformedIn[-NewIn, -In, +Out](chain: PhasesChain[In, Out], transformIn: NewIn => In) extends PhasesChain[NewIn, Out] {
  private[phase] def processWithProgress(progress: MultiPhasedProgress): NewIn => Out = { newIn =>
    val in = transformIn(newIn)
    chain.processWithProgress(progress)(in)
  }

  val phasesDetails = chain.phasesDetails
}

private[phase] class ChainWithTransformedOut[-In, +Out, +NewOut](chain: PhasesChain[In, Out], transformOut: Out => NewOut) extends PhasesChain[In, NewOut] {
  private[phase] def processWithProgress(progress: MultiPhasedProgress): In => NewOut = { in =>
    val out = chain.processWithProgress(progress)(in)
    transformOut(out)
  }

  val phasesDetails = chain.phasesDetails
}

private[phase] class WrappingChain[-NewIn, In, +Out, +NewOut](chain: PhasesChain[In, Out], wrap: NewIn => (In => Out) => NewOut) extends PhasesChain[NewIn, NewOut] {
  private[phase] def processWithProgress(progress: MultiPhasedProgress): NewIn => NewOut = { in =>
    wrap(in)(chain.processWithProgress(progress))
  }

  val phasesDetails = chain.phasesDetails
}