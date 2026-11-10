package server.lobby

import protocol.LobbyApi.{ApiError, AssignRequest, TableAddress, TableStarting}
import server.auth.{AuthenticatedEndpoint, SessionTokens}
import server.funds.FundsService
import utils.ModelExports.Game

/** `POST /assign`: a logged in player asks where to play. The lobby only says where, then the client talks to the table
 * directly.
 * A player who cannot afford the minimum bet is refused with 403 and has to deposit first.
 * The token is never logged. */
final class TableAssignmentEndpoint(dispatcher: TableDispatcher, tokens: SessionTokens, funds: FundsService)
  extends AuthenticatedEndpoint[AssignRequest]("Table assignment", tokens):
  import TableAssignmentEndpoint.*

  protected def tokenOf(request: AssignRequest): String = request.token

  protected def answerFor(username: String, request: AssignRequest): (Int, String) =
    // the check comes first: a refused player must not take a seat, nor make the lobby start a new table
    if funds.canPlay(username) then reply(username, dispatcher.assign())
    else
      System.out.println(s"No seat for $username: the balance is below the minimum bet")
      (403, json(ApiError(NotEnoughMoney)))

  private def reply(username: String, assignment: Assignment): (Int, String) = assignment match
    case Assignment.Assigned(table)          =>
      System.out.println(s"$username assigned to table ${table.id}")
      (200, json(TableAddress(table.id, table.host, table.wsPort)))
    case Assignment.Starting(tableId)        =>
      System.out.println(s"No seat for $username: table $tableId is starting")
      (202, json(TableStarting(tableId)))
    case Assignment.Full                     =>
      System.out.println(s"No seat for $username: the server is full")
      (503, json(ApiError(ServerFull)))
    case Assignment.ActivationFailed(tableId) =>
      System.out.println(s"No seat for $username: table $tableId could not be started")
      (503, json(ApiError(CannotStart)))

object TableAssignmentEndpoint:
  val InvalidToken = AuthenticatedEndpoint.InvalidToken
  val ServerFull   = "The server is full, try again later"
  val CannotStart  = "A new table could not be started, try again later"
  val NotEnoughMoney = s"You need at least ${Game.MinGameBet} fiches to sit at a table: make a deposit first"
