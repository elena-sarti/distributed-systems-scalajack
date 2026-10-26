package server

import protocol.{ClientMessage, ServerMessage}

/** Placeholder handler until the table session exists (C1): refuses every request. */
object NotReadyHandler extends SessionHandler:
  val Reason = "The table is not ready yet"

  def onConnected(connection: Connection): Unit = ()
  def onMessage(connection: Connection, message: ClientMessage): Unit =
    connection.send(ServerMessage.ErrorMessage(Reason))
  def onDisconnected(connection: Connection): Unit = ()
