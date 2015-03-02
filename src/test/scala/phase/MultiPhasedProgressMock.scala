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

class MultiPhasedProgressMock extends MultiPhasedProgress {

  var movedProgresses = 0
  private var _phasesIns  = List[String]()
  private var _phasesOuts = List[String]()
  def phasesIns  = _phasesIns.reverse
  def phasesOuts = _phasesOuts.reverse

  override protected def skipPhase(phaseDetails: PhaseDetails): Unit = {
    movedProgresses += 1
  }

  override protected def beginPhase(phaseDetails: PhaseDetails): Unit = {
    _phasesIns  = phaseDetails.name :: _phasesIns
  }

  override protected def endPhase(phaseDetails: PhaseDetails): Unit = {
    _phasesOuts = phaseDetails.name :: _phasesOuts
  }

  override private[phase] def finish(result: Any): Unit = {}

}
