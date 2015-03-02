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

import scala.util.control.NonFatal

private[phase] class ProgressActor[T](service: BackgroundService[T]) extends Actor {
  private val serviceActor = context.actorOf(StartingServiceActor.props(service), "background_service")

  private var state = ProgressState()

  def receive = {
    case start: StartService[_] =>
      updateStateHandlingErrors(state.start())
      serviceActor ! start
    case Init(totalPhases) =>
      updateStateHandlingErrors(state.init(totalPhases))
    case BeginPhase(details) =>
      updateStateHandlingErrors(state.beginPhase(details))
    case EndPhase(details) =>
      updateStateHandlingErrors(state.endPhase(details))
    case Finish(result) =>
      updateStateHandlingErrors(state.finish(result))
    case Reset =>
      state = ProgressState()
    case GetStatus =>
      sender ! state.status
  }

  private def updateStateHandlingErrors(changeState: => ProgressState) {
    state = try {
      changeState
    } catch {
      case NonFatal(ex) => state.fail(ex)
    }
  }

  override def supervisorStrategy = {
    OneForOneStrategy() {
      case NonFatal(ex) =>
        state = state.fail(ex)
        SupervisorStrategy.defaultDecider(ex)
    }
  }
}

object ProgressActor {
  def props[T](service: BackgroundService[T]): Props = Props(new ProgressActor(service))
}

private[phase] class StartingServiceActor[T](service: BackgroundService[T]) extends Actor {
  def receive = {
    case StartService(request) =>
      service.start(request.asInstanceOf[T], sender())
  }
}

private[phase] object StartingServiceActor {
  def props[T](service: BackgroundService[T]): Props = Props(new StartingServiceActor(service))
}

private[phase] case class StartService[T](request: T)

private[phase] case class Init(totalPhases: Seq[PhaseDetails])

private[phase] case class BeginPhase(details: PhaseDetails)

private[phase] case class EndPhase(details: PhaseDetails)

private[phase] case class Finish(result: Any)

case object Reset

case object GetStatus

trait BackgroundService[T]{
  def start(request: T, progressActor: ActorRef): Unit
}