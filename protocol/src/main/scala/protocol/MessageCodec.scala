package protocol

import upickle.default.{ReadWriter, read, write}
import scala.util.Try

/** Single entry point to turn protocol messages into JSON text and back.
 *
 * Every message becomes a self-contained JSON string, sent as one WebSocket text frame:
 * since WebSocket already delimits messages, no extra framing (e.g. a length prefix) is needed.
 * Decoding never throws: data coming from the network is untrusted, so a malformed
 * message becomes a `Left` with the reason, and the caller decides how to react.
 */
object MessageCodec:

  /** Serializes a client message to JSON. */
  def encode(message: ClientMessage): String = write(message)

  /** Serializes a server message to JSON. */
  def encode(message: ServerMessage): String = write(message)

  /** Deserializes a JSON string received by a table node.
   *
   * @param json the raw text received from the network.
   * @return the decoded message, or a description of why it is not a valid client message.
   */
  def decodeClientMessage(json: String): Either[String, ClientMessage] = decode[ClientMessage](json)

  /** Deserializes a JSON string received by a client.
   *
   * @param json the raw text received from the network.
   * @return the decoded message, or a description of why it is not a valid server message.
   */
  def decodeServerMessage(json: String): Either[String, ServerMessage] = decode[ServerMessage](json)

  private def decode[T: ReadWriter](json: String): Either[String, T] =
    Try(read[T](json)).toEither.left.map(error => s"Malformed message: ${error.getMessage}")
