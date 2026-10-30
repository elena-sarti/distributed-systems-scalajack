package server

import protocol.{ClientMessage, ServerMessage}

/** A client connected to the node, seen as an abstract channel: the table logic never depends on WebSocket. */
trait Connection:
  def id: String
  def send(message: ServerMessage): Unit

/** What the node does when something happens on a connection. */
trait SessionHandler:
  def onConnected(connection: Connection): Unit
  def onMessage(connection: Connection, message: ClientMessage): Unit
  def onDisconnected(connection: Connection): Unit

  /** Called periodically by the single writer, on the same thread as the other events, so that the handler
   * can react to the passing of time (e.g. a player who does not answer). By default nothing happens. */
  def onTick(): Unit = ()