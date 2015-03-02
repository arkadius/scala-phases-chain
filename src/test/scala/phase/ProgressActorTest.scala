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

import akka.actor._
import akka.testkit._
import org.scalatest._

class ProgressActorTest extends TestKit(ActorSystem("ProgressActorTest"))
  with fixture.FlatSpecLike
  with ImplicitSender
  with Matchers {

  class FixtureParam {
    private val service = new BackgroundServiceMock
    private val ref = system.actorOf(ProgressActor.props(service))

    def startService() = {
      ref ! StartService()
    }

    def addPhase(i: Int) = {
      service.addPhase(i)
    }

    def executeOnServiceAndWaitAWhile(f: BackgroundServiceMock => Unit) = {
      f(service)
      Thread.sleep(100)
    }

    def expectStatus(status: OperationStatus) = {
      ref ! GetStatus
      expectMsg(status)
    }
  }

  override protected def withFixture(test: OneArgTest): Outcome = {
    val result = test(new FixtureParam)
    system.shutdown()
    result
  }

  it should "do operation in progress" in { fixture =>
    fixture.expectStatus(NotStarted)

    fixture.startService()
    fixture.expectStatus(InProgress("Preparing", 0))

    fixture.addPhase(123)
    fixture.addPhase(234)
    fixture.addPhase(345)
    fixture.expectStatus(InProgress("Preparing", 0))

    fixture.executeOnServiceAndWaitAWhile(_.unlockPreparation())
    fixture.expectStatus(InProgress("123", 25)) // preparing phase is also counted

    fixture.executeOnServiceAndWaitAWhile(_.unlockPhase(123))
    fixture.expectStatus(InProgress("234", 50))

    fixture.executeOnServiceAndWaitAWhile(_.unlockPhase(234))
    fixture.expectStatus(InProgress("345", 75))

    fixture.executeOnServiceAndWaitAWhile(_.unlockPhase(345))
    fixture.expectStatus(Completed(345))
  }

}