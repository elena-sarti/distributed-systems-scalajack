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

/** How the primary sends its new states to the backup. */
trait Replicator:
  /** True only when the backup has stored the update: until then nobody may see the new state. */
  def replicate(update: ReplicaUpdate): Boolean
