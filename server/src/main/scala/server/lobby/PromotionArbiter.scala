package server.lobby

/** Tells whether a table node that sends its epoch to the lobby has not been replaced. */
trait EpochCheck:
  def admits(tableId: String, epoch: Long): Boolean

object EpochCheck:
  /** For the tables that are not replicated, which send no epoch. Nothing is refused. */
  val AdmitAll: EpochCheck = (_, _) => true

/** What the lobby decides about a replica that asks to change the configuration of its table. */
enum PromotionDecision:
  /** The request is accepted, and the epoch is now the current one for the table. */
  case Granted(epoch: Long)
  /** The lobby still hears the primary, so it is the backup that cannot reach it. */
  case PrimaryAlive
  /** The epoch asked for is not newer than the last one granted for this table. */
  case StaleEpoch(current: Long)
  /** The table is not in the registry. */
  case UnknownTable

/** The lobby as the third vote of the decision record. A backup is promoted only if the lobby also hears nothing from the
 * primary, otherwise two primaries could play the same table. It keeps the last epoch granted for every table and
 * refuses a request whose epoch is not greater, which is the fencing token.
 *
 * @param onGranted called after each promotion, outside the lock, with the table id.
 */
final class PromotionArbiter(registry: TableRegistry, onGranted: String => Unit = _ => ()) extends EpochCheck:
  private var epochs = Map.empty[String, Long]

  def decide(tableId: String, epoch: Long): PromotionDecision =
    val decision = judge(tableId, epoch)
    decision match
      case PromotionDecision.Granted(_) => onGranted(tableId)
      case _                            => ()
    decision

  /** The primary asks to go on without its backup: it is granted if the epoch is newer than the current one, which is shared
   * with the promotions so that a promotion and an exclusion cannot both win. */
  def exclude(tableId: String, epoch: Long): PromotionDecision = synchronized:
    registry.find(tableId) match
      case None    => PromotionDecision.UnknownTable
      case Some(_) => advance(tableId, epoch)

  /** A replicated table node asks to be the primary: granted with the next epoch unless another node plays the table now,
   * so a restarted primary does not take the table back from its promoted backup. */
  def start(tableId: String): PromotionDecision = synchronized:
    registry.find(tableId) match
      case Some(table) if table.status == TableStatus.Available => PromotionDecision.PrimaryAlive
      case _                                                    => advance(tableId, epochs.getOrElse(tableId, 0L) + 1)

  /** Whether a table node with this epoch may register and send heartbeats. An older epoch is refused and a newer one is
   * recorded. */
  def admits(tableId: String, epoch: Long): Boolean = synchronized:
    val current = epochs.getOrElse(tableId, 0L)
    if epoch > current then epochs = epochs.updated(tableId, epoch)
    epoch >= current

  /** Checks and records in one atomic step so that two requests cannot both win. */
  private def judge(tableId: String, epoch: Long): PromotionDecision = synchronized:
    registry.find(tableId) match
      case None                                                   => PromotionDecision.UnknownTable
      case Some(table) if table.status != TableStatus.Unavailable => PromotionDecision.PrimaryAlive
      case Some(_)                                                => advance(tableId, epoch)

  /** Grants the epoch if it is newer than the current one. Called with the lock held. */
  private def advance(tableId: String, epoch: Long): PromotionDecision =
    val current = epochs.getOrElse(tableId, 0L)
    if epoch <= current then PromotionDecision.StaleEpoch(current)
    else
      epochs = epochs.updated(tableId, epoch)
      PromotionDecision.Granted(epoch)
