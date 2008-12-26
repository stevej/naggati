package net.lag.naggati

// has to have a parameter so they don't become the same object:
sealed abstract case class StepResult(val name: String)
case object NEED_DATA extends StepResult("need-data")
case object COMPLETE extends StepResult("complete")


/**
 * Abstract base class for a "step" in a state machine. Steps are executed
 * when called (via `apply`) and return either
 * `NEED_DATA` if more data must be buffered before the step can
 * complete, or `COMPLETE` if the step has processed the data
 * buffered so far and processing should move on to the next step.
 *
 * Steps can be put into an implicit order via the `::` operator,
 * but many of the built-in steps (in the `Steps` object) take
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
