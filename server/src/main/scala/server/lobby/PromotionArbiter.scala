package server.lobby

/** What the lobby decides about a replica that asks to change the configuration of its table. */
enum PromotionDecision:
  case Granted(epoch: Long)
  /** The lobby still hears the primary: the table is not abandoned, the backup is the one that cannot reach it
   * (only for a promotion). */
  case PrimaryAlive
  /** The epoch asked for is not newer than the last one granted for this table. */
  case StaleEpoch(current: Long)
  case UnknownTable

/** The lobby as the third vote of the decision record: a backup is promoted only when two of the three nodes agree
 * that the primary is gone, i.e. the backup (which asks, because it hears nothing) and the lobby (which must hear
 * nothing either, i.e. the table is marked unavailable in the registry by the heartbeat monitor, E2).
 * If the lobby still hears the primary the answer is no: the backup is just cut off from it, and a second primary
 * would split the brain.
 *
 * It also keeps, for every table, the last epoch granted: a request with an epoch that is not greater is refused, so
 * the same promotion cannot be granted twice and an old request cannot undo a newer one (the epoch is the fencing token).
 *
 * @param onGranted called (outside the lock) after each promotion, with the table id.
 */
final class PromotionArbiter(registry: TableRegistry, onGranted: String => Unit = _ => ()):
  private var epochs = Map.empty[String, Long]

  def decide(tableId: String, epoch: Long): PromotionDecision =
    val decision = judge(tableId, epoch)
    decision match
      case PromotionDecision.Granted(_) => onGranted(tableId)
      case _                            => ()
    decision

  /** The primary cannot reach its backup and asks to go on alone (rows 3 and 4 of the decision record): the primary,
   * which is asking, and the lobby, which hears it, are two of three. Granted when the epoch is newer than the current
   * one. The epochs are the same as for the promotions, so a promotion and an exclusion asked for at the same time
   * cannot both win: the backup and the primary ask with the same epoch, the epoch of the primary plus one. A primary
   * that has already been replaced is refused for the same reason. And once the backup is out, it still has the old
   * epoch: it can no longer be promoted, since it does not have the moves played from now on. */
  def exclude(tableId: String, epoch: Long): PromotionDecision = synchronized:
    registry.find(tableId) match
      case None    => PromotionDecision.UnknownTable
      case Some(_) => advance(tableId, epoch)

  /** Check and record in one atomic step, so that two requests cannot both win. */
  private def judge(tableId: String, epoch: Long): PromotionDecision = synchronized:
    registry.find(tableId) match
      case None => PromotionDecision.UnknownTable
      case Some(table) if table.status != TableStatus.Unavailable => PromotionDecision.PrimaryAlive
      case Some(_) => advance(tableId, epoch)

  /** The new epoch, if it is newer than the current one. Called with the lock held. */
  private def advance(tableId: String, epoch: Long): PromotionDecision =
    val current = epochs.getOrElse(tableId, 0L)
    if epoch <= current then PromotionDecision.StaleEpoch(current)
    else
      epochs = epochs.updated(tableId, epoch)
      PromotionDecision.Granted(epoch)
