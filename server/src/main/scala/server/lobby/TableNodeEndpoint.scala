package server.lobby

import protocol.LobbyApi
import protocol.LobbyApi.{ApiError, TableAck, TableHeartbeat, TableRegistration}
import server.auth.JsonPostEndpoint
import upickle.default.ReadWriter
import scala.util.chaining.*

/** What the lobby does when a table node calls it: decode the body, change the registry, answer.
 * The two calls (registration, heartbeat) differ only in the registry operation, so the rest is here (DRY).
 *
 * @tparam Request the body sent by the node.
 */
abstract class TableNodeEndpoint[Request: ReadWriter](operation: String) extends JsonPostEndpoint(operation):

  /** The change to make in the registry, and the table as it is afterwards. */
  protected def execute(request: Request): Either[RegistryError, TableInfo]

  protected final def answer(body: String): (Int, String) =
    LobbyApi.decode[Request](body) match
      case Left(message)  => (400, json(ApiError(message)))
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

/** `POST /tables/register`: a table node announces itself. */
final class TableRegistrationEndpoint(registry: TableRegistry) extends TableNodeEndpoint[TableRegistration]("Table registration"):
  protected def execute(request: TableRegistration): Either[RegistryError, TableInfo] =
    registry.register(request.id, request.host, request.wsPort).tap: result =>
      result.foreach(table => println(s"Table ${table.id} registered at ${table.host}:${table.wsPort}"))

/** `POST /tables/heartbeat`: a table node says it is alive and how many seats are free. */
final class TableHeartbeatEndpoint(registry: TableRegistry) extends TableNodeEndpoint[TableHeartbeat]("Table heartbeat"):
  protected def execute(request: TableHeartbeat): Either[RegistryError, TableInfo] =
    registry.heartbeat(request.id, request.freeSeats)
