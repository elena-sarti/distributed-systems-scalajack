package server.replication

import protocol.LobbyApi
import upickle.default.ReadWriter
import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.time.Duration
import scala.util.control.NonFatal

/** The backup reached over HTTP (see [[ReplicaEndpoint]] and [[PrimaryHeartbeatEndpoint]]).
 *
 * @param backupUrl the address of the backup node.
 * @param timeout   how long the table waits for the backup, during which nobody at the table can play.
 */
final class HttpReplicator(backupUrl: String, timeout: Duration = Duration.ofSeconds(2)) extends Replicator, BackupHeartbeats:
  private val http         = HttpClient.newBuilder().connectTimeout(timeout).build()
  private val updateUri    = URI.create(backupUrl.stripSuffix("/") + ReplicaEndpoint.Path)
  private val heartbeatUri = URI.create(backupUrl.stripSuffix("/") + PrimaryHeartbeatEndpoint.Path)

  /** Whether the last update went through, so that only the changes are logged. */
  private var reachable = true

  def replicate(update: ReplicaUpdate): Boolean =
    val status = post(updateUri, update)
    val stored = status.contains(200)
    if stored != reachable then
      reachable = stored
      if stored then System.out.println("The backup is storing the table again")
      else System.err.println(s"The backup did not store the table (${describe(status)}): the table is paused")
    stored

  /** Runs on the thread of the heartbeats and does not touch `reachable`, which belongs to the thread of the table.
   * Failures are not logged here since the failure detector signals them. */
  def heartbeat(epoch: Long): Boolean = post(heartbeatUri, ReplicaHeartbeat(epoch)).contains(200)

  private def describe(status: Option[Int]): String = status match
    case Some(409)   => "it follows a newer primary"
    case Some(other) => s"status $other"
    case None        => "no answer"

  /** The status of the answer, or `None` if there was none. */
  private def post[T: ReadWriter](uri: URI, body: T): Option[Int] =
    try
      val request = HttpRequest.newBuilder(uri)
        .timeout(timeout)
        .header("Content-Type", "application/json")
        .POST(HttpRequest.BodyPublishers.ofString(LobbyApi.encode(body)))
        .build()
      Some(http.send(request, HttpResponse.BodyHandlers.discarding()).statusCode())
    catch case NonFatal(_) => None
