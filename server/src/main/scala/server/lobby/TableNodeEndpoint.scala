package server.lobby

import protocol.LobbyApi
import protocol.LobbyApi.{ApiError, TableAck, TableHeartbeat, TableRegistration}
import server.auth.JsonPostEndpoint
import upickle.default.ReadWriter
import scala.util.chaining.*

/** What the lobby does when a table node calls it: decode the body, change the registry, answer.
 * The two calls (registration, heartbeat) differ only in the registry operation, so the rest is here (DRY).
 *
 * A replicated table sends its epoch too: a node whose epoch is older than the current one has been replaced by
 * another node (e.g. it was cut off and its backup was promoted), so it gets 409 and the registry is not touched.
 *
 * @tparam Request the body sent by the node.
 */
abstract class TableNodeEndpoint[Request: ReadWriter](operation: String, epochs: EpochCheck) extends JsonPostEndpoint(operation):

  /** The change to make in the registry, and the table as it is afterwards. */
  protected def execute(request: Request): Either[RegistryError, TableInfo]

  /** The table and the epoch the node says it has, if it is replicated. */
  protected def epochOf(request: Request): Option[(String, Long)]

  protected final def answer(body: String): (Int, String) =
    LobbyApi.decode[Request](body) match
      case Left(message)  => (400, json(ApiError(message)))
      case Right(request) if !epochOf(request).forall(epochs.admits) =>
        (409, json(ApiError(TableNodeEndpoint.Superseded)))
      case Right(request) =>
        execute(request).fold(
          error => failure(error),
          table => (200, json(TableAck(table.id)))
        )

  /** Unknown table: 404, so the node knows it has to register again. Anything else is a wrong request: 400. */
  private def failure(error: RegistryError): (Int, String) = error match
    case RegistryError.UnknownTable(id)         => (404, json(ApiError(s"Unknown table '$id'")))
    case RegistryError.InvalidTable(reason)     => (400, json(ApiError(reason)))
    case RegistryError.InvalidFreeSeats(f, max) => (400, json(ApiError(s"The free seats must be between 0 and $max, not $f")))

object TableNodeEndpoint:
  val Superseded = "Another node is playing this table with a newer epoch"

/** `POST /tables/register`: a table node announces itself. */
final class TableRegistrationEndpoint(registry: TableRegistry, epochs: EpochCheck = EpochCheck.AdmitAll)
  extends TableNodeEndpoint[TableRegistration]("Table registration", epochs):
  protected def epochOf(request: TableRegistration): Option[(String, Long)] = request.epoch.map(request.id -> _)

  protected def execute(request: TableRegistration): Either[RegistryError, TableInfo] =
    registry.register(request.id, request.host, request.wsPort).tap: result =>
      result.foreach(table => System.out.println(s"Table ${table.id} registered at ${table.host}:${table.wsPort}"))

/** `POST /tables/heartbeat`: a table node says it is alive and how many seats are free. */
final class TableHeartbeatEndpoint(registry: TableRegistry, epochs: EpochCheck = EpochCheck.AdmitAll)
  extends TableNodeEndpoint[TableHeartbeat]("Table heartbeat", epochs):
  protected def epochOf(request: TableHeartbeat): Option[(String, Long)] = request.epoch.map(request.id -> _)

  protected def execute(request: TableHeartbeat): Either[RegistryError, TableInfo] =
    registry.heartbeat(request.id, request.freeSeats)
