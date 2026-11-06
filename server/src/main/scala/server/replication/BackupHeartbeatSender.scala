package server.replication

import server.liveness.{FailureDetector, Peer}

/** What the primary does at every period to keep its backup informed and to know if it is alive: sends it a
 * heartbeat and, if the backup answers, tells the detector. `tick` must be called by one thread at a time (see
 * `PeriodicTask`).
 *
 * @param epoch the epoch of this primary, sent in every heartbeat.
 */
final class BackupHeartbeatSender(backup: BackupHeartbeats, epoch: Long, detector: FailureDetector[Peer]):
  def tick(): Unit =
    if backup.heartbeat(epoch) then detector.heardFrom(Peer.Backup) // no answer: the detector will notice the silence
