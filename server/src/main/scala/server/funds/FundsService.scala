package server.funds

import server.db.{BalanceRepository, FundsError}
import utils.ModelExports.Game
import java.math.BigDecimal as JBigDecimal

/** Why a balance cannot be read, or money cannot be deposited or withdrawn. */
enum FundsRefusal:
  /** The amount is not a positive number of fiches within the limits . */
  case InvalidAmount
  /** The user does not exist. */
  case UnknownUser
  /** The player is sitting at a table, which holds their money until they stand up. */
  case SeatedAtTable
  /** The balance is lower than the amount to withdraw. */
  case InsufficientFunds

/** The money of the players while they are in the lobby, with balance, deposit and withdrawal. It checks the amount and
 * leaves to the repository what must be atomic. It is stateless and uses fiches, so a deposit just creates them. */
final class FundsService(balances: BalanceRepository):
  import FundsService.*

  def balance(username: String): Either[FundsRefusal, Double] =
    balances.balanceOf(username).toRight(FundsRefusal.UnknownUser)

  def deposit(username: String, amount: Double): Either[FundsRefusal, Double] =
    validated(amount).flatMap(balances.deposit(username, _).left.map(refusal))

  def withdraw(username: String, amount: Double): Either[FundsRefusal, Double] =
    validated(amount).flatMap(balances.withdraw(username, _).left.map(refusal))

  /** Whether the player has at least the minimum bet, as the table requires to sit down. */
  def canPlay(username: String): Boolean = balances.balanceOf(username).exists(_ >= Game.MinGameBet)

  private def validated(amount: Double): Either[FundsRefusal, Double] =
    Either.cond(isAmountValid(amount), amount, FundsRefusal.InvalidAmount)

  private def refusal(error: FundsError): FundsRefusal = error match
    case FundsError.UnknownUser       => FundsRefusal.UnknownUser
    case FundsError.Seated(_)         => FundsRefusal.SeatedAtTable
    case FundsError.InsufficientFunds => FundsRefusal.InsufficientFunds

object FundsService:
  /** The most that can be moved at once, well below what the `balance` column can hold. */
  val MaxAmount = 1_000_000.0

  /** Whether the amount is finite, positive, with at most two decimals and not above [[MaxAmount]]. */
  def isAmountValid(amount: Double): Boolean =
    amount.isFinite && amount > 0 && amount <= MaxAmount && JBigDecimal.valueOf(amount).scale <= 2
