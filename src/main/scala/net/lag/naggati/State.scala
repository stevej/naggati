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
import org.apache.mina.core.session.IoSession
import org.apache.mina.filter.codec.ProtocolDecoderOutput


object State {
  val EMPTY_BUFFER = IoBuffer.wrap(new Array[Byte](0))
}

/**
 * Container for state associated with a Mina `IoSession`,
 * including the current incoming data buffer and arbitrary key/value data
 * used by the decoding logic. A state object is associated with a session
 * when the session is first created, and stays attached as long as the
 * session is alive.
 */
class State protected[naggati](firstStep: Step, val session: IoSession, val out: ProtocolDecoderOutput) {
  /**
   * Current (mina) buffer being processed.
   */
  var buffer: IoBuffer = State.EMPTY_BUFFER

  // track whether we allocated this IoBuffer or it was passed in from mina
  private[naggati] var dynamicBuffer = false

  private[naggati] var currentStep = firstStep

  /**
   * Next step to execute after the current step completes.
   */
  var nextStep: Step = End

  /**
   * Arbitrary key/value data that can be used by steps to track any
   * ongoing state.
   */
  val data: mutable.Map[String, Any] = new mutable.HashMap[String, Any]


  /**
   * Add an IoBuffer from mina into our current state. If our current
   * buffer is empty, we optimistically store the buffer from mina,
   * in the hopes that it can be completely processed inline. If we have
   * leftovers, though, we build our own cumulative buffer.
   *
   * After this method returns, `buffer` is in flipped mode and contains
   * any previous data plus the new data.
   */
  private[naggati] def addBuffer(in: IoBuffer) = {
    // buffers from mina always arrive in "flipped" mode.
    if (buffer.position > 0) {
      if (!dynamicBuffer) {
        // well then, make it dynamic!
        val oldBuffer = buffer
        buffer = IoBuffer.allocate(oldBuffer.position + in.limit, false)
        buffer.setAutoExpand(true)
        // mina 2.0.0-M2 has bug here!: FIXME:
//      buffer.setAutoShrink(true)
        dynamicBuffer = true

        oldBuffer.flip
        buffer.put(oldBuffer)
      }
      buffer.put(in)
      buffer.flip
    } else {
      buffer = in
      dynamicBuffer = false
    }
  }

  /**
   * Retrieve a named value from the state map in `data`.
   */
  def apply[T](key: String) = data(key).asInstanceOf[T]

  /**
   * Set a named value in the state map in `data`.
   */
  def update(key: String, value: Any) = data(key) = value

  /**
   * Clear all current state, return to the first step of the state machine,
   * and discard any buffered data. This is probably only useful for unit
   * tests.
   */
  def reset = {
    data.clear
    currentStep = firstStep
    nextStep = End
    buffer = State.EMPTY_BUFFER
    dynamicBuffer = false
  }


  trait IntMapping {
    def apply(key: String): Int
    def update(key: String, value: Int)
  }

  private val intMapping = new IntMapping {
    def apply(key: String): Int = data.get(key) match {
      case None => 0
      case Some(x) => x.asInstanceOf[Int]
    }
    def update(key: String, value: Int) = data(key) = value
  }

  /**
   * Return a view of the key/value data store (in `data`) where
   * the values are all ints that default to 0 if not present yet.
   */
  def asInt = intMapping
}
