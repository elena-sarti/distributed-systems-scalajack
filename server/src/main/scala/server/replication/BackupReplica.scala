package server.replication

/** The copy of the table kept by the backup: the last state received from the primary.
 *
 * Called by the threads of the HTTP server, hence the lock. Updates are normally sent one at a time, but a
 * request the primary has given up on (timeout) can still arrive after a newer one: the sequence number lets
 * the backup ignore it instead of going back in time.
 */
final class BackupReplica:
  private var last: Option[ReplicaUpdate] = None
  private var closed = false

  def latest: Option[ReplicaUpdate] = synchronized(last)

  /** Whether a primary with this epoch is the one the backup follows (or a newer one): the heartbeats of a
   * primary that has been replaced must not count as signs of life. Also true before the first update. */
  def follows(epoch: Long): Boolean = synchronized(!closed && last.forall(_.epoch <= epoch))

  /** The backup has become the primary: it follows nobody any more, so whatever the old primary sends (even a
   * primary that restarted with a newer epoch) is refused and cannot change the state it took over. */
  def close(): Unit = synchronized:
    closed = true

  /** Keeps the update if it is the newest one seen so far. False if it comes from a primary older than the one
   * the backup already follows: that primary has been replaced and must not be acknowledged. */
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
