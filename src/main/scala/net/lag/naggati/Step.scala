package net.lag.naggati

// has to have a parameter so they don't become the same object:
sealed abstract case class StepResult(val name: String)
case object NEED_DATA extends StepResult("need-data")
case object COMPLETE extends StepResult("complete")


/**
 * Abstract base class for a "step" in a state machine. Steps are executed
 * when called (via <code>apply</code>) and return either
 * <code>NEED_DATA</code> if more data must be buffered before the step can
 * complete, or <code>COMPLETE</code> if the step has processed the data
 * buffered so far and processing should move on to the next step.
 *
 * <p>
 * Steps can be put into an implicit order via the <code>::</code> operator,
 * but many of the built-in steps (in the <code>Steps</code> object) take
 * a code block as a parameter, with the code block providing the next Step
 * to execute.
 */
abstract class Step {
  // an implicit (default) next-step can be set via the :: operator
  private[naggati] var next: Step = End

  def apply(): StepResult

  // s1 :: s2  -->  s1 then s2
  def ::(s: Step) = { s.next = this; s }

  /**
   * Return the current state object for the current decoder.
   */
  protected def state = Decoder.localState.get()
}

/**
 * Special Step which means "end of decoding; start over".
 */
final object End extends Step {
  override def apply(): StepResult = COMPLETE
}


// FIXME: move these

class ReadDelimiterStep(getDelimiter: () => Byte, process: (Int) => Step) extends Step {
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

// when you know the delimiter ahead of time, this is probably faster.
class ReadNDelimiterStep(delimiter: Byte, process: (Int) => Step) extends Step {
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
