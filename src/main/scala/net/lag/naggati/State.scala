package net.lag.naggati

import scala.collection.mutable
import org.apache.mina.core.buffer.IoBuffer
import org.apache.mina.core.session.IoSession
import org.apache.mina.filter.codec.ProtocolDecoderOutput


/**
 * Container for state associated with a Mina <code>IoSession</code>,
 * including the current incoming data buffer and arbitrary key/value data
 * used by the decoding logic. A state object is associated with a session
 * when the session is first created, and stays attached as long as the
 * session is alive.
 */
class State protected[naggati](firstStep: Step, val session: IoSession, val out: ProtocolDecoderOutput) {
  /**
   * Current buffer being processed.
   */
  var buffer: IoBuffer = IoBuffer.EMPTY_BUFFER

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
  val data: mutable.Map[String, Object] = new mutable.HashMap[String, Object]


  /**
   * Add an IoBuffer from mina into our current state. If our current
   * buffer is empty, we optimistically just store the buffer from mina,
   * in the hopes that it can be completely processed inline. If we have
   * leftovers, though, we build our own cumulative buffer.
   *
   * After this method returns, 'buffer' is in flipped mode and contains
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
   * Retrieve a named value from the state map in <code>data</code>.
   */
  def apply(key: String) = data(key)

  /**
   * Set a named value in the state map in <code>data</code>.
   */
  def update(key: String, value: Object) = data(key) = value

  /**
   * Clear all current state, return to the first step of the state machine,
   * and discard any buffered data. This is probably only useful for unit
   * tests.
   */
  def reset = {
    data.clear
    currentStep = firstStep
    nextStep = End
    buffer = IoBuffer.EMPTY_BUFFER
    dynamicBuffer = false
  }
}
