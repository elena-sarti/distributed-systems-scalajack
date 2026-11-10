package server

import protocol.{ClientMessage, ServerMessage}

/** A client connected to the node, seen as an abstract channel so that the table logic does not depend on WebSocket. */
trait Connection:
  def id: String
  def send(message: ServerMessage): Unit

/** What the node does when something happens on a connection. */
trait SessionHandler:
  def onConnected(connection: Connection): Unit
  def onMessage(connection: Connection, message: ClientMessage): Unit
  def onDisconnected(connection: Connection): Unit

  /** Called periodically by the single writer, on the same thread as the other events, so that the handler can react to
   * the passing of time. */
  def onTick(): Unit = ()

  /** Called once by the single writer when the node shuts down, after the events already queued.  */
  def onStop(): Unit = ()

/** What a dispatcher needs to know about a table, which is how many seats are free. */
trait SeatAvailability:
  def freeSeats: Int
