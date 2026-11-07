package server.lobby

import protocol.LobbyApi
import protocol.LobbyApi.{ApiError, LocateRequest, TableAddress}
import server.auth.{JsonPostEndpoint, SessionTokens}
import server.db.BalanceRepository

/** `POST /tables/locate`: a logged in player who lost the connection with their table (e.g. its node failed and the
 * backup took over) asks where the table is now. The lobby answers with the address the table has in its registry, which
 * is the one of the node that plays it at this moment, so the client does not need to know that a failover happened.
 *
 * Which table the player sits at is read from the database, where the table that has the player's seat has put it:
 * the player does not tell it, so nobody can ask for the address of someone else's table.
 * As for `/assign`, the token is checked here and never written in the log.
 */
final class TableLocationEndpoint(balances: BalanceRepository, registry: TableRegistry, tokens: SessionTokens)
  extends JsonPostEndpoint("Table location"):
  import TableLocationEndpoint.*

  protected def answer(body: String): (Int, String) =
    LobbyApi.decode[LocateRequest](body) match
      case Left(message)  => (400, json(ApiError(message)))
      case Right(request) =>
        tokens.verify(request.token) match
          case None           => (401, json(ApiError(InvalidToken)))
          case Some(username) => reply(balances.tableOf(username))

  private def reply(tableId: Option[String]): (Int, String) = tableId match
    case None => (404, json(ApiError(NotSeated)))
    case Some(id) =>
      registry.find(id) match
        case Some(table) if table.status == TableStatus.Available => (200, json(TableAddress(table.id, table.host, table.wsPort)))
        case _ => (503, json(ApiError(NotAvailable))) // unknown to the lobby, or silent: its backup may be taking over

object TableLocationEndpoint:
  val InvalidToken = "Invalid or expired session token"
  val NotSeated    = "You are not seated at any table"
  val NotAvailable = "Your table is not reachable yet, try again in a moment"
