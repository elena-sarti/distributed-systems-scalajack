package server.replication

import server.liveness.{FailureDetector, Peer, PeerStatus}
import server.{ExclusionGateway, PromotionAnswer}

/** What a primary does when it stops hearing its backup: asks the lobby to go on without it, and if the lobby agrees
 * (the primary and the lobby are two of three: rows 3 and 4 of the decision record) the table plays on alone, with a
 * new epoch. Until then the table is paused, because no move can be replicated: the CP choice.
 *
 * When the backup answers again (a heartbeat with the new epoch, which it accepts because it is newer than its own)
 * the table takes it back: it sends it the whole state and then waits for it at every move, as before. The lobby does
 * not need to agree: the backup becomes eligible for a promotion again only because it now has the current epoch.
 *
 * `tick` is called at every period by the liveness task, so the request is repeated for as long as the backup is
 * suspected and the lobby has not agreed (the lobby may be unreachable for a while). A refusal with 409 means that
 * the epoch of this primary is old: the backup has been promoted in the meantime, and this table stays paused.
 */
final class BackupExcluder(tableId: String, table: ReplicatedTable, peers: FailureDetector[Peer], lobby: ExclusionGateway):
  private var lastAnswer: Option[PromotionAnswer] = None // to log only when the answer changes, not at every period

  def tick(): Unit = synchronized:
    if table.isAlone && peers.status(Peer.Backup).contains(PeerStatus.Alive) then
      lastAnswer = None
      table.takeBackupBack()
    else if !table.isAlone && peers.isSuspected(Peer.Backup) then
      val epoch  = table.currentEpoch + 1
      val answer = lobby.requestExclusion(tableId, epoch)
      if !lastAnswer.contains(answer) then
        lastAnswer = Some(answer)
        System.out.println(s"[table $tableId] going on without the backup (epoch $epoch): $answer")
      if answer == PromotionAnswer.Granted then table.goOnAlone(epoch)
