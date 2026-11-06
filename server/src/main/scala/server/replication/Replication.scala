package server.replication

import upickle.default.ReadWriter

/** A new state of the table, sent by the primary to the backup.
 *
 * @param epoch    identifies the primary that sent it: a later primary always has a greater epoch, so the backup
 *                 can refuse whatever still comes from one that has been replaced.
 * @param sequence the position of this state among those sent by the same primary.
 */
final case class ReplicaUpdate(epoch: Long, sequence: Long, snapshot: TableSnapshot) derives ReadWriter

/** The answer of the backup to an update it has stored. */
final case class ReplicaAck(epoch: Long, sequence: Long) derives ReadWriter

/** A sign of life of the primary, sent to the backup every period even when the table is idle (an idle table
 * replicates nothing, so without it the backup could not tell a quiet primary from a dead one).
 * The epoch lets the backup ignore a primary that has been replaced. */
final case class ReplicaHeartbeat(epoch: Long) derives ReadWriter

/** The answer of the backup to a heartbeat it has accepted. */
final case class HeartbeatAck(epoch: Long) derives ReadWriter

/** How the primary says to the backup that it is alive. Separate from [[Replicator]]: it is a different job, with a
 * different rhythm, and the replication does not need to know about it (ISP). */
trait BackupHeartbeats:
  /** True only if the backup answered: an answer is also what tells the primary that the backup is alive.
   * Never throws. */
  def heartbeat(epoch: Long): Boolean

/** How the primary sends its new states to the backup. */
trait Replicator:
  /** True only when the backup has stored the update: until then nobody may see the new state. */
  def replicate(update: ReplicaUpdate): Boolean
