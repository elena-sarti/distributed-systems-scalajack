package server.lobby

import protocol.LobbyApi.{ApiError, LocateRequest, TableAddress}
import server.auth.{AuthenticatedEndpoint, SessionTokens}
import server.db.BalanceRepository

/** `POST /locate`: a logged in player who lost the connection with their table asks where it is now. The lobby answers
 * with the address of the node that plays it, so the client does not notice a failover. The table is read from the
 * database, so nobody can locate someone else's table. The token is checked here and never logged. */
final class TableLocationEndpoint(balances: BalanceRepository, registry: TableRegistry, tokens: SessionTokens)
  extends AuthenticatedEndpoint[LocateRequest]("Table location", tokens):
  import TableLocationEndpoint.*

  protected def tokenOf(request: LocateRequest): String = request.token

  protected def answerFor(username: String, request: LocateRequest): (Int, String) = reply(balances.tableOf(username))

  private def reply(tableId: Option[String]): (Int, String) = tableId match
    case None     => (404, json(ApiError(NotSeated)))
    case Some(id) =>
      registry.find(id) match
        case Some(table) if table.status == TableStatus.Available => (200, json(TableAddress(table.id, table.host, table.wsPort)))
        case _                                                    => (503, json(ApiError(NotAvailable))) // unknown to the lobby, or silent: its backup may be taking over

object TableLocationEndpoint:
  val InvalidToken = AuthenticatedEndpoint.InvalidToken
  val NotSeated    = "You are not seated at any table"
  val NotAvailable = "Your table is not reachable yet, try again in a moment"
