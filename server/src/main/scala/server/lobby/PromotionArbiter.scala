package server.lobby

/** What the lobby decides about a backup that asks to take the place of its primary. */
enum PromotionDecision:
  case Granted(epoch: Long)
  /** The lobby still hears the primary: the table is not abandoned, the backup is the one that cannot reach it. */
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

  /** Check and record in one atomic step, so that two requests cannot both win. */
  private def judge(tableId: String, epoch: Long): PromotionDecision = synchronized:
    registry.find(tableId) match
      case None => PromotionDecision.UnknownTable
      case Some(table) if table.status != TableStatus.Unavailable => PromotionDecision.PrimaryAlive
      case Some(_) =>
        val current = epochs.getOrElse(tableId, 0L)
        if epoch <= current then PromotionDecision.StaleEpoch(current)
        else
          epochs = epochs.updated(tableId, epoch)
          PromotionDecision.Granted(epoch)
