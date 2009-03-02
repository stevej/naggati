/*
 * Copyright 2009 Robey Pointer <robeypointer@lag.net>
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may
 * not use this file except in compliance with the License. You may obtain
 * a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package net.lag.naggati

import org.apache.mina.core.session.{DummySession, IoSession}
import org.specs._
import scala.actors.Actor
import scala.collection.mutable


object IoHandlerActorAdapterSpec extends Specification {

  private var fakeSession: IoSession = null

  case object Reset
  case object Get

  val actor = Actor.actor {
    val history = new mutable.ListBuffer[AnyRef]

    Actor.loop {
      Actor.receive {
        case Reset => history.clear()
        case Get => Actor.reply(history.toList)
        case x: AnyRef => history += x
      }
    }
  }

  "IoHandlerActorAdapter" should {
    doBefore {
      fakeSession = new DummySession
      actor ! Reset
    }


    "create a default filter on a session" in {
      val handler = new IoHandlerActorAdapter(s => actor)
      handler.sessionCreated(fakeSession)
      fakeSession.getAttribute(IoHandlerActorAdapter.sessionInfo.KEY) mustEqual SessionInfo(Some(actor), MinaMessage.defaultFilter)
    }

    "skip messages that are being filtered out" in {
      val handler = new IoHandlerActorAdapter(s => actor)
      handler.sessionCreated(fakeSession)
      IoHandlerActorAdapter.filter(fakeSession) -= classOf[MinaMessage.MessageSent]
      fakeSession.getAttribute(IoHandlerActorAdapter.sessionInfo.KEY) mustEqual SessionInfo(Some(actor), MinaMessage.defaultFilter - classOf[MinaMessage.MessageSent])

      handler.messageReceived(fakeSession, "hello")
      handler.messageSent(fakeSession, "goodbye")
      val history = actor !? Get
      history mustEqual List(MinaMessage.MessageReceived("hello"))
    }
  }
}
