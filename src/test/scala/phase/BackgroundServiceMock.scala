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

import akka.actor.ActorRef

class BackgroundServiceMock extends BackgroundService[Unit] {

  private var _phases: List[WaitingPhase] = Nil


  private var preparationLocked: Boolean = true

  def addPhase(i: Int): Unit = {
    _phases = new WaitingPhase(i) :: _phases
  }

  def unlockPreparation(): Unit = {
    synchronized {
      preparationLocked = false
      notifyAll()
    }
  }

  def unlockPhase(i: Int): Unit = {
    _phases.find(_.i == i).foreach(_.unlock())
  }

  override def start(request: Unit, progressActor: ActorRef): Unit = {
    val runner = ChainRunner.actorBased(prepareChain, progressActor)
    runner.run()
  }

  protected def prepareChain: PhasesChain[Unit, Int] = {
    synchronized {
      while (preparationLocked) {
        wait()
      }
    }
    _phases.reverseMap(_.chain).reduceRight(_ :: _)
  }
}

class WaitingPhase(val i: Int) {

  private var locked: Boolean = true

  def chain: PhasesChain[Any, Int] = SinglePhaseChain(i.toString) { _ =>
    WaitingPhase.this.synchronized {
      while (locked) {
        WaitingPhase.this.wait()
      }
    }
    i
  }


  def unlock(): Unit = {
    synchronized {
      locked = false
      notifyAll()
    }
  }

}
