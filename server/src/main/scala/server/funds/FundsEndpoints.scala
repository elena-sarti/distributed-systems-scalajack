package server.funds

import protocol.LobbyApi
import protocol.LobbyApi.{AmountRequest, ApiError, BalanceRequest, Funds}
import server.auth.{AuthenticatedEndpoint, SessionTokens}

/** `POST /deposit` and `POST /withdraw`: the same request and the same answers, only the operation changes, so it is
 * given to the endpoint (DRY). 200 with the new balance; 400, 404 or 409 with the reason (see [[FundsEndpoint.answer]]).
 *
 * @param operation what the endpoint does, for the log (e.g. "Deposit").
 * @param move      the operation on the funds of a user: [[FundsService.deposit]] or [[FundsService.withdraw]].
 */
final class FundsEndpoint(operation: String, tokens: SessionTokens, move: (String, Double) => Either[FundsRefusal, Double])
  extends AuthenticatedEndpoint[AmountRequest](operation, tokens):

  protected def tokenOf(request: AmountRequest): String = request.token

  protected def answerFor(username: String, request: AmountRequest): (Int, String) =
    FundsEndpoint.answer(move(username, request.amount))

object FundsEndpoint:
  val InvalidAmount     = s"The amount must be a positive number of fiches, with at most two decimals, up to ${FundsService.MaxAmount.toLong}"
  val UnknownUser       = "This user does not exist"
  val SeatedAtTable     = "You are sitting at a table: leave it before moving your money"
  val InsufficientFunds = "Your balance is lower than the amount to withdraw"

  /** The answer to a request on the funds: shared by the three endpoints, so a refusal has the same status and reason
   * everywhere. */
  private[funds] def answer(result: Either[FundsRefusal, Double]): (Int, String) = result match
    case Right(balance)                       => (200, LobbyApi.encode(Funds(balance)))
    case Left(FundsRefusal.InvalidAmount)     => (400, error(InvalidAmount))
    case Left(FundsRefusal.UnknownUser)       => (404, error(UnknownUser))
    case Left(FundsRefusal.SeatedAtTable)     => (409, error(SeatedAtTable))
    case Left(FundsRefusal.InsufficientFunds) => (409, error(InsufficientFunds))

  private def error(reason: String): String = LobbyApi.encode(ApiError(reason))

/** `POST /balance`: the balance of the player. While they are sitting at a table it is the one they brought there:
 * the table gives the rest back when they stand up. */
final class BalanceEndpoint(tokens: SessionTokens, funds: FundsService)
  extends AuthenticatedEndpoint[BalanceRequest]("Balance", tokens):

  protected def tokenOf(request: BalanceRequest): String = request.token

  protected def answerFor(username: String, request: BalanceRequest): (Int, String) =
    FundsEndpoint.answer(funds.balance(username))
