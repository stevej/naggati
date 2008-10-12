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
  def step(f: () => StepResult): Step = new Step { def apply() = { f() } }


  private class ReadBytesStep(getCount: () => Int, process: () => Step) extends Step {
    def apply(): StepResult = {
      val count = getCount()
      if (state.buffer.limit - state.buffer.position < count) {
        NEED_DATA
      } else {
        state.nextStep = process()
        COMPLETE
      }
    }
  }

  private class ReadNBytesStep(count: Int, process: () => Step) extends Step {
    def apply(): StepResult = {
      if (state.buffer.limit - state.buffer.position < count) {
        NEED_DATA
      } else {
        state.nextStep = process()
        COMPLETE
      }
    }
  }

  private class ReadDelimiterStep(getDelimiter: () => Byte, process: (Int) => Step) extends Step {
    def apply(): StepResult = {
      val delimiter = getDelimiter()
      state.buffer.indexOf(delimiter) match {
        case -1 =>
          NEED_DATA
        case n =>
          state.nextStep = process(n - state.buffer.position + 1)
          COMPLETE
      }
    }
  }

  private class ReadNDelimiterStep(delimiter: Byte, process: (Int) => Step) extends Step {
    def apply(): StepResult = {
      state.buffer.indexOf(delimiter) match {
        case -1 =>
          NEED_DATA
        case n =>
          state.nextStep = process(n - state.buffer.position + 1)
          COMPLETE
      }
    }
  }

  private class ReadUntilStep(filter: (Byte) => Boolean, process: (Int) => Step) extends Step {
    def apply(): StepResult = {
      val prev = state.asInt("_until")
      var i = state.buffer.position + prev
      while (i < state.buffer.limit) {
        if (filter(state.buffer.get(i))) {
          state.nextStep = process(i - state.buffer.position + 1)
          state.asInt("_until") = 0
          return COMPLETE
        }
        i += 1
      }
      NEED_DATA
    }
  }


  /**
   * Ensure that a certain number of bytes is buffered before executing the
   * next step, calling <code>getCount</code> each time new data arrives, to
   * recompute the total number of bytes desired. If the desired number of
   * bytes is a constant, the other <code>readBytes</code> below (which
   * takes a constant int argument) may be faster.
   */
  def readBytes(getCount: () => Int)(process: () => Step): Step =
    new ReadBytesStep(getCount, process)

  /**
   * Ensure that at least <code>count</code> bytes are buffered before
   * executing the next processing step.
   */
  def readBytes(count: Int)(process: () => Step): Step =
    new ReadNBytesStep(count, process)

  /**
   * Read a certain number of bytes into a byte buffer and pass that buffer
   * to the next step in processing.
   * <code>getCount</code> is called each time new data arrives, to recompute
   * the total number of bytes desired.
   * The creation and copying of a temporary byte buffer may have a small
   * performance penalty.
   */
  def readByteBuffer(getCount: () => Int)(process: Array[Byte] => Step): Step =
    new ReadBytesStep(getCount, { () =>
      val byteBuffer = new Array[Byte](getCount())
      state.buffer.get(byteBuffer)
      process(byteBuffer)
    })

  /**
   * Read <code>count</code> bytes into a byte buffer and pass that buffer
   * to the next step in processing.
   * The creation and copying of a temporary byte buffer may have a small
   * performance penalty.
   */
  def readByteBuffer(count: Int)(process: Array[Byte] => Step): Step =
    new ReadNBytesStep(count, { () =>
      val byteBuffer = new Array[Byte](count)
      state.buffer.get(byteBuffer)
      process(byteBuffer)
    })

  /**
   * Read bytes until a delimiter is present. The number of bytes up to and
   * including the delimiter is passed to the next processing step.
   * <code>getDelimiter</code> is called each time new data arrives.
   */
  def readDelimiter(getDelimiter: () => Byte)(process: (Int) => Step): Step =
    new ReadDelimiterStep(getDelimiter, process)

  /**
   * Read bytes until a delimiter is present. The number of bytes up to and
   * including the delimiter is passed to the next processing step.
   */
  def readDelimiter(delimiter: Byte)(process: (Int) => Step): Step =
    new ReadNDelimiterStep(delimiter, process)

  /**
   * Read bytes until a delimiter is present, and pass a buffer containing
   * the bytes up to and including the delimiter to the next processing step.
   * <code>getDelimiter</code> is called each time new data arrives.
   * The creation and copying of a temporary byte buffer may have a small
   * performance penalty.
   */
  def readDelimiterBuffer(getDelimiter: () => Byte)(process: (Array[Byte]) => Step): Step =
    new ReadDelimiterStep(getDelimiter, (n: Int) => {
      val byteBuffer = new Array[Byte](n)
      state.buffer.get(byteBuffer)
      process(byteBuffer)
    })

  /**
   * Read bytes until a delimiter is present, and pass a buffer containing
   * the bytes up to and including the delimiter to the next processing step.
   * The creation and copying of a temporary byte buffer may have a small
   * performance penalty.
   */
  def readDelimiterBuffer(delimiter: Byte)(process: (Array[Byte]) => Step): Step =
    new ReadNDelimiterStep(delimiter, (n: Int) => {
      val byteBuffer = new Array[Byte](n)
      state.buffer.get(byteBuffer)
      process(byteBuffer)
    })

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
    new ReadNDelimiterStep('\n'.toByte, (n) => {
      val end = if ((n > 1) && (state.buffer.get(state.buffer.position + n - 2) == '\r'.toByte)) {
        n - 2
      } else {
        n - 1
      }
      val byteBuffer = new Array[Byte](n)
      state.buffer.get(byteBuffer)
      process(new String(byteBuffer, 0, (if (removeLF) end else n), encoding))
    })

  /**
   * Read a line, terminated by LF or CRLF, and pass that line as a string
   * (decoded using UTF-8) to the next processing step.
   *
   * @param removeLF true if the LF or CRLF should be stripped from the
   *   string before passing it on; false to leave the line terminator
   *   attached
   */
  def readLine(removeLF: Boolean)(process: (String) => Step): Step = readLine(removeLF, "UTF-8")(process)

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
  def readUntil(filter: (Byte) => Boolean)(process: (Int) => Step): Step =
    new ReadUntilStep(filter, process)

  /**
   * Read a single byte and pass in on to the next step.
   */
  def readInt8(process: (Byte) => Step): Step = new ReadNBytesStep(1, { () => process(state.buffer.get) })

  // read 4-byte ints:
  def readInt32(process: (Int) => Step): Step = readInt32BE(process)
  def readInt32BE(process: (Int) => Step): Step =
    new ReadNBytesStep(4, { () =>
      state.buffer.order(ByteOrder.BIG_ENDIAN)
      process(state.buffer.getInt)
    })
  def readInt32LE(process: (Int) => Step): Step =
    new ReadNBytesStep(4, { () =>
      state.buffer.order(ByteOrder.LITTLE_ENDIAN)
      process(state.buffer.getInt)
    })
}
