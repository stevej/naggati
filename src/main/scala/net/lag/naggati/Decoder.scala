/*
 * Copyright 2009 Robey Pointer <robeypointer@gmail.com>
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

import scala.collection.mutable
import org.apache.mina.core.buffer.IoBuffer
import org.apache.mina.core.filterchain.IoFilter
import org.apache.mina.core.session.{DummySession, IoSession}
import org.apache.mina.filter.codec._


/**
 * Convenience exception class to allow decoders to indicate a protocol
 * error.
 */
class ProtocolError(message: String) extends Exception(message)


object Decoder {
  protected[naggati] var localState = new ThreadLocal[State]
}


/**
 * Mina ProtocolDecoder that interacts with state-machine Steps.
 */
class Decoder(private val firstStep: Step) extends ProtocolDecoder {

  private val STATE_KEY = "com.twitter.tomservo.state".intern

  def dispose(session: IoSession): Unit = {
    session.removeAttribute(STATE_KEY)
  }

  @throws(classOf[Exception])
  def finishDecode(session: IoSession, out: ProtocolDecoderOutput): Unit = {
    // won't do any good.
  }

  def stateFor(session: IoSession, out: ProtocolDecoderOutput) = {
    session.getAttribute(STATE_KEY) match {
      case null =>
        val newState = new State(firstStep, session, out)
        session.setAttribute(STATE_KEY, newState)
        newState
      case x => x.asInstanceOf[State]
    }
  }

  @throws(classOf[Exception])
  def decode(session: IoSession, in: IoBuffer, out: ProtocolDecoderOutput): Unit = {
    val state = stateFor(session, out)
    state.addBuffer(in)

    // stuff the decoder state into a thread-local so that codec steps can reach it easily.
    Decoder.localState.set(state)

    var done = false
    do {
      val step = state.currentStep
      step() match {
        case NEED_DATA =>
          // stay in current state; collect more data; try again later.
          done = true

        case COMPLETE =>
          /* if there's a next step set in the state, use that.
           * otherwise if there's an implicit next step after the
           * current one, use that. if nothing else, repeat the
           * first step.
           */
          state.currentStep = state.nextStep match {
            case End => step.next match {
              case End => firstStep
              case s => s
            }
            case s => s
          }
          state.nextStep = End
      }
    } while (! done)

    state.buffer.compact
    state.buffer.limit(state.buffer.position)
  }
}


// for use in unit tests
class TestDecoder {
  val fakeDecoderOutput = new ProtocolDecoderOutput {
    override def flush(nextFilter: IoFilter.NextFilter, s: IoSession) = {}
    override def write(obj: AnyRef) = written += obj
  }

  val fakeSession: IoSession = new DummySession
  val written = new mutable.ListBuffer[AnyRef]

  def apply(buffer: IoBuffer, firstStep: Step): List[AnyRef] = {
    written.clear()
    val decoder = new Decoder(firstStep)
    decoder.decode(fakeSession, buffer, fakeDecoderOutput)
    written.toList
  }

  def apply(s: String, firstStep: Step): List[AnyRef] = apply(s.getBytes, firstStep)
  def apply(x: Array[Byte], firstStep: Step): List[AnyRef] = apply(IoBuffer.wrap(x), firstStep)

  def write(obj: AnyRef) = fakeDecoderOutput.write(obj)
}
