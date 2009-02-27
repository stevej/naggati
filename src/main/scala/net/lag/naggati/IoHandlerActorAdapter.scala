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

import scala.actors.Actor
import scala.collection.immutable
import org.apache.mina.core.service.IoHandler
import org.apache.mina.core.session.{IdleStatus, IoSession}
import net.lag.logging.Logger


/**
 * All messages sent to an actor in reference to a Mina `IoSession`
 * will be a subclass of `MinaMessage`.
 */
abstract sealed class MinaMessage
object MinaMessage {
  case object SessionOpened extends MinaMessage
  case class MessageReceived(message: AnyRef) extends MinaMessage
  case class MessageSent(message: AnyRef) extends MinaMessage
  case class ExceptionCaught(cause: Throwable) extends MinaMessage
  case class SessionIdle(status: IdleStatus) extends MinaMessage
  case object SessionClosed extends MinaMessage

  type Filter = Set[Class[_ <: MinaMessage]]

  val defaultFilter: Filter = immutable.Set(
    // FIXME: is there a better way to get an object's class?
    MinaMessage.SessionOpened.getClass.asInstanceOf[Class[MinaMessage]],
    classOf[MinaMessage.MessageReceived],
    classOf[MinaMessage.MessageSent],
    classOf[MinaMessage.ExceptionCaught],
    classOf[MinaMessage.SessionIdle],
    MinaMessage.SessionClosed.getClass.asInstanceOf[Class[MinaMessage]])
}


/**
 * Converts Mina `IoSession` events into messages to be sent to
 * an actor.
 */
class IoHandlerActorAdapter(val actorFactory: (IoSession) => Actor) extends IoHandler {

  private val log = Logger.get

  /**
   * Send a message to the actor associated with this session, if there is
   * one.
   */
  def send(session: IoSession, message: MinaMessage) = {
    for (actor <- IoHandlerActorAdapter.actorFor(session)) {
      if (IoHandlerActorAdapter.filterFor(session) contains message.getClass.asInstanceOf[Class[MinaMessage]]) {
        actor ! message
      }
    }
  }

  /**
   * Send a message to the actor associated with this session. If no actor
   * is associated with the session, run a block of code instead.
   */
  def sendOr(session: IoSession, message: => MinaMessage)(f: => Unit) = {
    IoHandlerActorAdapter.actorFor(session) match {
      case None => f
      case Some(actor) => actor ! message
    }
  }

  def sessionCreated(session: IoSession) = {
    // don't overwrite an existing actor
    IoHandlerActorAdapter.actorFor(session) match {
      case None => session.setAttribute(IoHandlerActorAdapter.ACTOR_KEY, actorFactory(session))
      case Some(_) =>
    }
  }

  def sessionOpened(session: IoSession) = send(session, MinaMessage.SessionOpened)
  def messageReceived(session: IoSession, message: AnyRef) = send(session, MinaMessage.MessageReceived(message))
  def messageSent(session: IoSession, message: AnyRef) = send(session, MinaMessage.MessageSent(message))

  def exceptionCaught(session: IoSession, cause: Throwable) = {
    sendOr(session, MinaMessage.ExceptionCaught(cause)) {
      // weird bad: an exception happened but i guess it wasn't associated with any existing session.
      log.error(cause, "Exception inside mina!")
    }
  }

  def sessionIdle(session: IoSession, status: IdleStatus) = send(session, MinaMessage.SessionIdle(status))

  def sessionClosed(session: IoSession) = {
    send(session, MinaMessage.SessionClosed)
    session.removeAttribute(IoHandlerActorAdapter.ACTOR_KEY)
  }
}


object IoHandlerActorAdapter {
  private val ACTOR_KEY = "scala.mina.actor".intern
  private val FILTER_KEY = "scala.mina.filter".intern

  /**
   * Return the actor associated with a Mina session, if any.
   * An actor is created for each new Mina session automatically by the
   * factory passed to an `IoHandlerActorAdapter`, or can be
   * set manually by `setActorFor`.
   */
  def actorFor(session: IoSession): Option[Actor] = {
    val actor = session.getAttribute(ACTOR_KEY).asInstanceOf[Actor]
    if (actor == null) None else Some(actor)
  }

  /**
   * Manually set the actor that should receive I/O event messages for a
   * given Mina `IoSession`.
   */
  def setActorFor(session: IoSession, actor: Actor) = session.setAttribute(ACTOR_KEY, actor)
  
  def filterFor(session: IoSession) = {
    session.getAttribute(FILTER_KEY, MinaMessage.defaultFilter).asInstanceOf[MinaMessage.Filter]
  }
}
