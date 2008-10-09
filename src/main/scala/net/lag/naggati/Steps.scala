package net.lag.naggati

import java.nio.ByteOrder


// FIXME this is the most important one to document!
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
  
  def readBytes(getCount: () => Int)(process: () => Step): Step =
    new ReadBytesStep(getCount, process)
  

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
  
  /**
   * Ensure that at least <code>count</code> bytes are buffered before
   * executing the next processing step.
   */
  def readBytes(count: Int)(process: () => Step): Step =
    new ReadNBytesStep(count, process)



  // this isn't very efficient (lots of buffer copying):
  def readByteBuffer(getCount: () => Int)(process: Array[Byte] => Step): Step =
    new ReadBytesStep(getCount, { () =>
      val byteBuffer = new Array[Byte](getCount())
      state.buffer.get(byteBuffer)
      process(byteBuffer)
    })
  def readByteBuffer(count: Int)(process: Array[Byte] => Step): Step =
    new ReadNBytesStep(count, { () =>
      val byteBuffer = new Array[Byte](count)
      state.buffer.get(byteBuffer)
      process(byteBuffer)
    })

  def readDelimiter(getDelimiter: () => Byte)(process: (Int) => Step) =
    new ReadDelimiterStep(getDelimiter, process)
  def readDelimiter(delimiter: Byte)(process: (Int) => Step) =
    new ReadNDelimiterStep(delimiter, process)

  // this isn't very efficient (lots of buffer copying):
  def readDelimiterBuffer(getDelimiter: () => Byte)(process: (Array[Byte]) => Step) =
    new ReadDelimiterStep(getDelimiter, (n: Int) => {
      val byteBuffer = new Array[Byte](n)
      state.buffer.get(byteBuffer)
      process(byteBuffer)
    })
  def readDelimiterBuffer(delimiter: Byte)(process: (Array[Byte]) => Step) =
    new ReadNDelimiterStep(delimiter, (n: Int) => {
      val byteBuffer = new Array[Byte](n)
      state.buffer.get(byteBuffer)
      process(byteBuffer)
    })

  // specialized for line buffering:
  def readLine(removeLF: Boolean)(process: (String) => Step) =
    new ReadNDelimiterStep('\n'.toByte, (n) => {
      val end = if ((n > 1) && (state.buffer.get(state.buffer.position + n - 2) == '\r'.toByte)) {
        n - 2
      } else {
        n - 1
      }
      val byteBuffer = new Array[Byte](n)
      state.buffer.get(byteBuffer)
      process(new String(byteBuffer, 0, (if (removeLF) end else n), "UTF-8"))
    })
  def readLine(process: (String) => Step): Step = readLine(true)(process)

  // read 1-byte ints:
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
