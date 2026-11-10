package server.replication

import server.liveness.{FailureDetector, Peer}

/** Sends a heartbeat to the backup at every period and, if the backup answers, tells the detector.
 * `tick` must be called by one thread at a time (see [[server.PeriodicTask]]).
 *
 * @param epoch the epoch of this primary, sent in every heartbeat and read at every tick since it grows when the primary
 *              goes on without its backup.
 */
final class BackupHeartbeatSender(backup: BackupHeartbeats, epoch: => Long, detector: FailureDetector[Peer]):
  def tick(): Unit =
    if backup.heartbeat(epoch) then detector.heardFrom(Peer.Backup) // no answer: the detector will notice the silence
