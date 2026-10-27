package network

import cats.effect.IO
import protocol.{ClientMessage, ServerMessage}

/** Everything the client can observe on the connection with its table.
 *
 * Network problems are modelled as events instead of exceptions, so the caller
 * handles them like any other piece of data coming from the server.
 */
enum ConnectionEvent:
  /** A valid message from the table node. */
  case Received(message: ServerMessage)
  /** A frame that is not a valid [[ServerMessage]]: reported, never thrown, as network data is untrusted. */
  case Invalid(reason: String)
  /** The connection is over (closed by the server, or lost). Nothing will follow. */
  case Disconnected(reason: String)

/** Raised by [[ServerConnection.send]] when the connection is already over. */
final class ConnectionClosedException extends RuntimeException("The connection with the table is closed")

/** The client's view of the network: write requests, read what the table says.
 *
 * It hides the transport, so the controller never depends on WebSocket.
 * Both operations are asynchronous: an `IO` is only a description of the operation,
 * and while it waits for the network no thread is blocked.
 */
trait ServerConnection:

  /** Sends a request to the table. The `IO` completes when the message has been handed
   * over for sending, not when the server has processed it: the answer arrives via [[receive]].
   *
   * Fails with [[ConnectionClosedException]] if the connection is over. */
  def send(message: ClientMessage): IO[Unit]

  /** Waits for the next event, suspending the caller until one is available.
   * After [[ConnectionEvent.Disconnected]] it keeps returning it. */
  def receive: IO[ConnectionEvent]

