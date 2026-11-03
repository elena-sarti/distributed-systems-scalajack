package server.replication

import protocol.LobbyApi
import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.time.Duration
import scala.util.control.NonFatal

/** The backup reached over HTTP (see [[ReplicaEndpoint]]).
 *
 * @param backupUrl the address of the backup node, e.g. `http://table-1-backup:8080`.
 * @param timeout   how long the table waits for the backup: while it waits, nobody at the table can play.
 */
final class HttpReplicator(backupUrl: String, timeout: Duration = Duration.ofSeconds(2)) extends Replicator:
  private val http     = HttpClient.newBuilder().connectTimeout(timeout).build()
  private val endpoint = URI.create(backupUrl.stripSuffix("/") + ReplicaEndpoint.Path)

  /** Whether the last update went through: only the changes are logged, not every failed attempt. */
  private var reachable = true

  def replicate(update: ReplicaUpdate): Boolean =
    val status = post(update)
    val stored = status.contains(200)
    if stored != reachable then
      reachable = stored
      if stored then System.out.println("The backup is storing the table again")
      else System.err.println(s"The backup did not store the table (${describe(status)}): the table is paused")
    stored

  private def describe(status: Option[Int]): String = status match
    case Some(409)   => "it follows a newer primary"
    case Some(other) => s"status $other"
    case None        => "no answer"

  private def post(update: ReplicaUpdate): Option[Int] =
    try
      val request = HttpRequest.newBuilder(endpoint)
        .timeout(timeout)
        .header("Content-Type", "application/json")
        .POST(HttpRequest.BodyPublishers.ofString(LobbyApi.encode(update)))
        .build()
      Some(http.send(request, HttpResponse.BodyHandlers.discarding()).statusCode())
    catch case NonFatal(_) => None
