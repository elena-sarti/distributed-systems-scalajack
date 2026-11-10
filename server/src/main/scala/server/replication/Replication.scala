package server.replication

import upickle.default.ReadWriter

/** A new state of the table, sent by the primary to the backup.
 *
 * @param epoch    identifies the primary that sent it. A later primary has a greater epoch, so the backup can refuse
 *                 whatever still comes from a replaced one.
 * @param sequence the position of this state among those sent by the same primary.
 * @param snapshot the state of the table.
 */
final case class ReplicaUpdate(epoch: Long, sequence: Long, snapshot: TableSnapshot) derives ReadWriter

/** The answer of the backup to an update it has stored. */
final case class ReplicaAck(epoch: Long, sequence: Long) derives ReadWriter

/** A sign of life of the primary, sent to the backup every period even when the table is idle. The epoch lets the backup
 * ignore a replaced primary. */
final case class ReplicaHeartbeat(epoch: Long) derives ReadWriter

/** The answer of the backup to a heartbeat it has accepted. */
final case class HeartbeatAck(epoch: Long) derives ReadWriter

/** How the primary tells the backup that it is alive. It is separate from [[Replicator]] since the job and the rhythm are
 * different (ISP). */
trait BackupHeartbeats:
  /** Returns true only if the backup answered, which also tells the primary that the backup is alive. Never throws. */
  def heartbeat(epoch: Long): Boolean

/** How the primary sends its new states to the backup. */
trait Replicator:
  /** Returns true only when the backup has stored the update, since until then nobody may see the new state. */
  def replicate(update: ReplicaUpdate): Boolean
