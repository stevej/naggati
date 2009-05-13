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

  def classOfObj[T <: AnyRef](x: T) = x.getClass.asInstanceOf[Class[T]]

  val defaultFilter: Filter = immutable.Set(
    classOfObj(MinaMessage.SessionOpened),
    classOf[MinaMessage.MessageReceived],
    classOf[MinaMessage.MessageSent],
    classOf[MinaMessage.ExceptionCaught],
    classOf[MinaMessage.SessionIdle],
    classOfObj(MinaMessage.SessionClosed))
}


case class SessionInfo(actor: Option[Actor], filter: MinaMessage.Filter)


/**
 * Converts Mina `IoSession` events into messages to be sent to
 * an actor.
 */
class IoHandlerActorAdapter(val actorFactory: (IoSession) => Actor) extends IoHandler {

  private val log = Logger.get

  // initialize the session and attach some state to it.
  def sessionCreated(session: IoSession) = {
    val info = IoHandlerActorAdapter.sessionInfo(session)
    // don't overwrite an existing actor
    IoHandlerActorAdapter.sessionInfo(session).actor match {
      case None =>
        val actor = actorFactory(session)
        // actor may have modified the SessionInfo filter
        val filter = IoHandlerActorAdapter.sessionInfo(session).filter
        IoHandlerActorAdapter.sessionInfo(session) = SessionInfo(if (actor == null) None else Some(actor), filter)
      case Some(_) =>
    }
  }

  /**
   * Send a message to the actor associated with this session, if there is
   * one.
   */
  def send(session: IoSession, message: MinaMessage) = {
    val info = IoHandlerActorAdapter.sessionInfo(session)
    for (actor <- info.actor; if info.filter contains MinaMessage.classOfObj(message)) {
      actor ! message
    }
  }

  /**
   * Send a message to the actor associated with this session. If no actor
   * is associated with the session, run a block of code instead.
   */
  def sendOr(session: IoSession, message: => MinaMessage)(f: => Unit) = {
    val info = IoHandlerActorAdapter.sessionInfo(session)
    info.actor match {
      case None => f
      case Some(actor) =>
        if (info.filter contains MinaMessage.classOfObj(message)) {
          actor ! message
        }
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
    IoHandlerActorAdapter.sessionInfo.remove(session)
  }
}


object IoHandlerActorAdapter {
  /**
   * Track state for each existing session by imitating a map.
   */
  object sessionInfo {
    val KEY = "scala.mina.session_info".intern

    def apply(session: IoSession): SessionInfo = {
      val info = session.getAttribute(KEY).asInstanceOf[SessionInfo]
      if (info == null) {
        val newInfo = SessionInfo(None, MinaMessage.defaultFilter)
        session.setAttribute(KEY, newInfo)
        newInfo
      } else {
        info
      }
    }

    def update(session: IoSession, info: SessionInfo): Unit = {
      session.setAttribute(KEY, info)
    }

    def remove(session: IoSession): Unit = {
      session.removeAttribute(KEY)
    }
  }

  /**
   * Manually set the actor that should receive I/O event messages for a
   * given Mina `IoSession`.
   */
  def setActorFor(session: IoSession, actor: Actor): Unit = {
    sessionInfo(session) = SessionInfo(Some(actor), sessionInfo(session).filter)
  }

  /**
   * Add or remove message types from the incoming-message filter by using
   * the `+=` and `-=` operators.
   */
  def filter(session: IoSession) = new {
    val info = sessionInfo(session)

    def +=(t: Class[_ <: MinaMessage]) = {
      sessionInfo(session) = SessionInfo(info.actor, info.filter + t)
    }

    def -=(t: Class[_ <: MinaMessage]) = {
      sessionInfo(session) = SessionInfo(info.actor, info.filter - t)
    }

    def +=[T <: MinaMessage](obj: T) = {
      sessionInfo(session) = SessionInfo(info.actor, info.filter + MinaMessage.classOfObj(obj))
    }

    def -=[T <: MinaMessage](obj: T) = {
      sessionInfo(session) = SessionInfo(info.actor, info.filter - MinaMessage.classOfObj(obj))
    }

    def +=(set: Set[Class[_ <: MinaMessage]]) = {
      sessionInfo(session) = SessionInfo(info.actor, info.filter ++ set)
    }

    def -=(set: Set[Class[_ <: MinaMessage]]) = {
      sessionInfo(session) = SessionInfo(info.actor, info.filter -- set)
    }
  }
}
