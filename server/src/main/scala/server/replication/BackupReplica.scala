package server.replication

/** The copy of the table kept by the backup, which is the last state received from the primary. The sequence number lets
 * it ignore a late request instead of going back in time. */
final class BackupReplica:
  private var last: Option[ReplicaUpdate] = None
  private var closed = false

  def latest: Option[ReplicaUpdate] = synchronized(last)

  /** Whether a primary with this epoch is the one the backup follows, or a newer one. The heartbeats of a replaced primary
   * must not count as signs of life. Also true before the first update. */
  def follows(epoch: Long): Boolean = synchronized(!closed && last.forall(_.epoch <= epoch))

  /** Called when the backup becomes the primary. It follows nobody any more, so whatever the old primary sends, even with a
   * newer epoch, is refused and cannot change the state taken over. */
  def close(): Unit = synchronized:
    closed = true

  /** Keeps the update if it is the newest seen so far. Returns false if it comes from a primary older than the one followed,
   * which has been replaced and must not be acknowledged. */
  def store(update: ReplicaUpdate): Boolean = synchronized:
    if closed then false
    else
      last match
        case Some(current) if update.epoch < current.epoch => false
        case Some(current) if update.epoch == current.epoch && update.sequence <= current.sequence =>
          true // already stored, or older than what is stored: nothing to do
        case _ =>
          last = Some(update)
          true
