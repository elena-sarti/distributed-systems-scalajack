package server.lobby

import protocol.LobbyApi
import protocol.LobbyApi.{ApiError, AssignRequest, TableAddress, TableStarting}
import server.auth.{JsonPostEndpoint, SessionTokens}

/** `POST /assign`: a logged in player asks where to play. The lobby only says where: from then on the
 * client talks to the table node directly, and no move of the game passes through the lobby.
 *
 * The token is checked here so that nobody can take seats without having logged in; the table checks it
 * again at the WebSocket handshake. The token is never written in the log.
 */
final class TableAssignmentEndpoint(dispatcher: TableDispatcher, tokens: SessionTokens)
  extends JsonPostEndpoint("Table assignment"):
  import TableAssignmentEndpoint.*

  protected def answer(body: String): (Int, String) =
    LobbyApi.decode[AssignRequest](body) match
      case Left(message)  => (400, json(ApiError(message)))
      case Right(request) =>
        tokens.verify(request.token) match
          case None           => (401, json(ApiError(InvalidToken)))
          case Some(username) => reply(username, dispatcher.assign())

  private def reply(username: String, assignment: Assignment): (Int, String) = assignment match
    case Assignment.Assigned(table) =>
      System.out.println(s"$username assigned to table ${table.id}")
      (200, json(TableAddress(table.id, table.host, table.wsPort)))
    case Assignment.Starting(tableId) =>
      System.out.println(s"No seat for $username: table $tableId is starting")
      (202, json(TableStarting(tableId)))
    case Assignment.Full =>
      System.out.println(s"No seat for $username: the server is full")
      (503, json(ApiError(ServerFull)))
    case Assignment.ActivationFailed(tableId) =>
      System.out.println(s"No seat for $username: table $tableId could not be started")
      (503, json(ApiError(CannotStart)))

object TableAssignmentEndpoint:
  val InvalidToken = "Invalid or expired session token"
  val ServerFull   = "The server is full, try again later"
  val CannotStart  = "A new table could not be started, try again later"
