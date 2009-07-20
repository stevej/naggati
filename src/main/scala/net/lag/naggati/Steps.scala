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

import java.nio.ByteOrder


/**
 * Common state-machine steps. These methods all generate a step, and
 * take a function (as their last argument) which will accept the output
 * of the step and return the next step to execute. In this way, processing
 * steps can be chained and remain readable.
 */
object Steps {
  /**
   * Return the current state object for this decoder. The state object can
   * be used to store arbitrary key/value data for the life of a session,
   * and also holds the buffer currently being processed.
   */
  def state: State = Decoder.localState.get()

  /**
   * Generate a Step from a code block.
   */
  def step(f: => StepResult): Step = new Step { def apply() = { f } }

  /**
   * Executes next step with all bytes in the buffer. This works best
   * when used with a connection-less protocol like UDP where everything
   * must come in one packet. This will not work well with TCP if fragmentation
   * has occurred; you might only see a partial packet.
   *
   * The creation and copying of a temporary byte buffer may have a small
   * performance penalty.
   */
  def readAll(process: Array[Byte] => Step) = step {
    if (state.buffer.limit - state.buffer.position == 0) {
      NEED_DATA
    } else {
      val byteBuffer = new Array[Byte](state.buffer.limit - state.buffer.position)
      state.buffer.get(byteBuffer)
      state.nextStep = process(byteBuffer)
      COMPLETE
    }
  }

  /**
   * Ensure that a certain number of bytes is buffered before executing the
   * next step, calling `getCount` each time new data arrives, to
   * recompute the total number of bytes desired. If the desired number of
   * bytes is a constant, the other `readBytes` below (which
   * takes a constant int argument) may be faster.
   */
  def readBytes(getCount: => Int)(process: => Step) = step {
    if (state.buffer.limit - state.buffer.position < getCount) {
      NEED_DATA
    } else {
      state.nextStep = process
      COMPLETE
    }
  }

  /**
   * Ensure that at least `count` bytes are buffered before
   * executing the next processing step.
   */
  def readBytes(count: Int)(process: => Step) = step {
    if (state.buffer.limit - state.buffer.position < count) {
      NEED_DATA
    } else {
      state.nextStep = process
      COMPLETE
    }
  }

  /**
   * Read a certain number of bytes into a byte buffer and pass that buffer
   * to the next step in processing.
   * `getCount` is called each time new data arrives, to recompute
   * the total number of bytes desired.
   * The creation and copying of a temporary byte buffer may have a small
   * performance penalty.
   */
  def readByteBuffer(getCount: => Int)(process: Array[Byte] => Step) = readBytes(getCount) {
    val byteBuffer = new Array[Byte](getCount)
    state.buffer.get(byteBuffer)
    process(byteBuffer)
  }

  /**
   * Read `count` bytes into a byte buffer and pass that buffer
   * to the next step in processing.
   * The creation and copying of a temporary byte buffer may have a small
   * performance penalty.
   */
  def readByteBuffer(count: Int)(process: Array[Byte] => Step) = readBytes(count) {
    val byteBuffer = new Array[Byte](count)
    state.buffer.get(byteBuffer)
    process(byteBuffer)
  }

  /**
   * Read bytes until a delimiter is present. The number of bytes up to and
   * including the delimiter is passed to the next processing step.
   * `getDelimiter` is called each time new data arrives.
   */
  def readDelimiter(getDelimiter: => Byte)(process: (Int) => Step) = step {
    state.buffer.indexOf(getDelimiter) match {
      case -1 =>
        NEED_DATA
      case n =>
        state.nextStep = process(n - state.buffer.position + 1)
        COMPLETE
    }
  }

  /**
   * Read bytes until a delimiter is present. The number of bytes up to and
   * including the delimiter is passed to the next processing step.
   */
  def readDelimiter(delimiter: Byte)(process: (Int) => Step) = step {
    state.buffer.indexOf(delimiter) match {
      case -1 =>
        NEED_DATA
      case n =>
        state.nextStep = process(n - state.buffer.position + 1)
        COMPLETE
    }
  }

  /**
   * Read bytes until a multi-byte delimiter is present, in its entirety.
   * The number of bytes up to and including the delimiter is passed to
   * the next processing step.
   */
  def readDelimiter(delimiter: Array[Byte])(process: (Int) => Step) = step {
    val oldpos = state.buffer.position
    var i = 0
    var done = false
    val check = new Array[Byte](delimiter.size)
    while (oldpos + i <= state.buffer.limit - delimiter.size && !done) {
      if (state.buffer.get(oldpos + i) == delimiter(0)) {
        state.buffer.position(oldpos + i)
        state.buffer.get(check)
        state.buffer.position(oldpos)
        if (delimiter deepEquals check) {
          state.nextStep = process(i + delimiter.size)
          done = true
        }
      }
      i += 1
    }
    if (done) COMPLETE else NEED_DATA
  }

  /**
   * Read bytes until a delimiter is present, and pass a buffer containing
   * the bytes up to and including the delimiter to the next processing step.
   * `getDelimiter` is called each time new data arrives.
   * The creation and copying of a temporary byte buffer may have a small
   * performance penalty.
   */
  def readDelimiterBuffer(getDelimiter: => Byte)(process: (Array[Byte]) => Step) =
    readDelimiter(getDelimiter) { n =>
      val byteBuffer = new Array[Byte](n)
      state.buffer.get(byteBuffer)
      process(byteBuffer)
    }

  /**
   * Read bytes until a delimiter is present, and pass a buffer containing
   * the bytes up to and including the delimiter to the next processing step.
   * The creation and copying of a temporary byte buffer may have a small
   * performance penalty.
   */
  def readDelimiterBuffer(delimiter: Byte)(process: (Array[Byte]) => Step) =
    readDelimiter(delimiter) { n =>
      val byteBuffer = new Array[Byte](n)
      state.buffer.get(byteBuffer)
      process(byteBuffer)
    }

  /**
   * Read bytes until a delimiter is present, and pass a buffer containing
   * the bytes up to and including the delimiter to the next processing step.
   * The creation and copying of a temporary byte buffer may have a small
   * performance penalty.
   */
  def readDelimiterBuffer(delimiter: Array[Byte])(process: (Array[Byte]) => Step) =
    readDelimiter(delimiter) { n =>
      val byteBuffer = new Array[Byte](n)
      state.buffer.get(byteBuffer)
      process(byteBuffer)
    }

  /**
   * Read a line, terminated by LF or CRLF, and pass that line as a string
   * to the next processing step.
   *
   * @param removeLF true if the LF or CRLF should be stripped from the
   *   string before passing it on; false to leave the line terminator
   *   attached
   * @param encoding byte-to-character encoding to use
   */
  def readLine(removeLF: Boolean, encoding: String)(process: (String) => Step): Step =
    readDelimiter('\n'.toByte) { n =>
      val end = if ((n > 1) && (state.buffer.get(state.buffer.position + n - 2) == '\r'.toByte)) {
        n - 2
      } else {
        n - 1
      }
      val byteBuffer = new Array[Byte](n)
      state.buffer.get(byteBuffer)
      process(new String(byteBuffer, 0, (if (removeLF) end else n), encoding))
    }

  /**
   * Read a line, terminated by LF or CRLF, and pass that line as a string
   * (decoded using UTF-8) to the next processing step.
   *
   * @param removeLF true if the LF or CRLF should be stripped from the
   *   string before passing it on; false to leave the line terminator
   *   attached
   */
  def readLine(removeLF: Boolean)(process: (String) => Step): Step =
    readLine(removeLF, "UTF-8")(process)

  /**
   * Read a line, terminated by LF or CRLF, and pass that line as a string
   * (decoded using UTF-8, with the line terminators stripped) to the next
   * processing step.
   */
  def readLine(process: (String) => Step): Step = readLine(true, "UTF-8")(process)

  /**
   * Read until a given condition is true on a byte in the buffer. Until a
   * byte passes the conditional filter, more data will be buffered. Once
   * the condition is true, the offset of the byte that matches the condition
   * will be passed to the next step.
   */
  def readUntil(filter: (Byte) => Boolean)(process: (Int) => Step) = step {
    val prev = state.asInt("_until")
    var i = state.buffer.position + prev
    var done = false
    while (i < state.buffer.limit && !done) {
      if (filter(state.buffer.get(i))) {
        state.nextStep = process(i - state.buffer.position + 1)
        state.asInt("_until") = 0
        done = true
      } else {
        i += 1
      }
    }
    if (done) COMPLETE else NEED_DATA
  }

  /**
   * Read a single byte and pass in on to the next step.
   */
  def readInt8(process: (Byte) => Step): Step = readBytes(1) { process(state.buffer.get) }

  /**
   * Read a 16-bit int and pass in on to the next step.
   */
  def readInt16(process: (Short) => Step): Step = readInt16BE(process)
  def readInt16BE(process: (Short) => Step): Step = readBytes(2) {
    state.buffer.order(ByteOrder.BIG_ENDIAN)
    process(state.buffer.getShort)
  }
  def readInt16LE(process: (Short) => Step): Step = readBytes(2) {
    state.buffer.order(ByteOrder.LITTLE_ENDIAN)
    process(state.buffer.getShort)
  }

  /**
   * Read a 32-bit int and pass in on to the next step.
   */
  def readInt32(process: (Int) => Step): Step = readInt32BE(process)
  def readInt32BE(process: (Int) => Step): Step = readBytes(4) {
    state.buffer.order(ByteOrder.BIG_ENDIAN)
    process(state.buffer.getInt)
  }
  def readInt32LE(process: (Int) => Step): Step = readBytes(4) {
    state.buffer.order(ByteOrder.LITTLE_ENDIAN)
    process(state.buffer.getInt)
  }

  /**
   * Read a 64-bit int and pass in on to the next step.
   */
  def readInt64(process: (Long) => Step): Step = readInt64BE(process)
  def readInt64BE(process: (Long) => Step): Step = readBytes(8) {
    state.buffer.order(ByteOrder.BIG_ENDIAN)
    process(state.buffer.getLong)
  }
  def readInt64LE(process: (Long) => Step): Step = readBytes(8) {
    state.buffer.order(ByteOrder.LITTLE_ENDIAN)
    process(state.buffer.getLong)
  }

  /**
   * Read a 64-bit double and pass in on to the next step.
   */
  def readDouble(process: (Double) => Step): Step = readDoubleBE(process)
  def readDoubleBE(process: (Double) => Step): Step = readBytes(8) {
    state.buffer.order(ByteOrder.BIG_ENDIAN)
    process(state.buffer.getDouble)
  }
  def readDoubleLE(process: (Double) => Step): Step = readBytes(8) {
    state.buffer.order(ByteOrder.LITTLE_ENDIAN)
    process(state.buffer.getDouble)
  }
}
