package server.funds

import server.db.{BalanceRepository, FundsError}
import utils.ModelExports.Game
import java.math.BigDecimal as JBigDecimal

/** Why a balance cannot be read, or money cannot be deposited or withdrawn. */
enum FundsRefusal:
  /** The amount is not a positive number of fiches within the limits (see [[FundsService.isAmountValid]]). */
  case InvalidAmount
  case UnknownUser
  /** The player is sitting at a table, which holds their money until they stand up. */
  case SeatedAtTable
  /** The balance is lower than the amount to withdraw. */
  case InsufficientFunds

/** The money of the players while they are in the lobby: reads the balance, deposits, withdraws.
 *
 * It checks the amount, and leaves to the repository what must be atomic (the player is not sitting, the balance
 * never goes below zero). Stateless, so thread-safe. There is no real payment behind it: the game is played with
 * fiches, and a deposit just creates them.
 */
final class FundsService(balances: BalanceRepository):
  import FundsService.*

  def balance(username: String): Either[FundsRefusal, Double] =
    balances.balanceOf(username).toRight(FundsRefusal.UnknownUser)

  def deposit(username: String, amount: Double): Either[FundsRefusal, Double] =
    validated(amount).flatMap(balances.deposit(username, _).left.map(refusal))

  def withdraw(username: String, amount: Double): Either[FundsRefusal, Double] =
    validated(amount).flatMap(balances.withdraw(username, _).left.map(refusal))

  /** Whether the player has the money to sit at a table: at least the minimum bet, the same rule the table applies. */
  def canPlay(username: String): Boolean = balances.balanceOf(username).exists(_ >= Game.MinGameBet)

  private def validated(amount: Double): Either[FundsRefusal, Double] =
    Either.cond(isAmountValid(amount), amount, FundsRefusal.InvalidAmount)

  private def refusal(error: FundsError): FundsRefusal = error match
    case FundsError.UnknownUser       => FundsRefusal.UnknownUser
    case FundsError.Seated(_)         => FundsRefusal.SeatedAtTable
    case FundsError.InsufficientFunds => FundsRefusal.InsufficientFunds

object FundsService:
  /** The most that can be moved at once: far from what the `balance` column (NUMERIC(12, 2)) can hold. */
  val MaxAmount = 1_000_000.0

  /** A finite, positive amount with at most two decimals (the cents of the column) and not above [[MaxAmount]]. */
  def isAmountValid(amount: Double): Boolean =
    amount.isFinite && amount > 0 && amount <= MaxAmount && JBigDecimal.valueOf(amount).scale <= 2
