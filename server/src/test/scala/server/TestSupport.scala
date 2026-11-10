package server

import protocol.ClientMessage
import java.util.concurrent.{BlockingQueue, LinkedBlockingQueue, TimeUnit}

/** Test double of the table logic: records what the endpoint reports and can answer messages. */
final class RecordingHandler(reply: (Connection, ClientMessage) => Unit = (_, _) => ()) extends SessionHandler:
  val connected    = LinkedBlockingQueue[Connection]()
  val received     = LinkedBlockingQueue[(Connection, ClientMessage)]()
  val disconnected = LinkedBlockingQueue[Connection]()

  def onConnected(connection: Connection): Unit = connected.put(connection)
  def onMessage(connection: Connection, message: ClientMessage): Unit =
    received.put((connection, message))
    reply(connection, message)
  def onDisconnected(connection: Connection): Unit = disconnected.put(connection)

object TestSupport:
  /** Takes the next element, failing the test (instead of hanging) if nothing arrives in time. */
  def next[T](queue: BlockingQueue[T]): T =
    Option(queue.poll(5, TimeUnit.SECONDS)).getOrElse(throw AssertionError("Timed out waiting for an event"))
