package server.replication

import server.liveness.{FailureDetector, Peer, PeerStatus}
import server.{ExclusionGateway, PromotionAnswer}

/** Lets a primary that cannot hear its backup ask the lobby to go on without it. Until the lobby agrees the table is
 * paused, which is the CP choice. When the backup answers again the table sends it the whole state and takes it back.
 * A 409 means this primary has been replaced, so it steps down.
 *
 * @param onReplaced called when this primary steps down.
 */
final class BackupExcluder(tableId: String, table: ReplicatedTable, peers: FailureDetector[Peer], lobby: ExclusionGateway,
                           onReplaced: () => Unit = () => ()):
  private var lastAnswer: Option[PromotionAnswer] = None // to log only when the answer changes, not at every period

  def tick(): Unit = synchronized:
    if table.isReplaced then ()
    else if table.isAlone && peers.status(Peer.Backup).contains(PeerStatus.Alive) then
      lastAnswer = None
      table.takeBackupBack()
    else if !table.isAlone && peers.isSuspected(Peer.Backup) then
      val epoch  = table.currentEpoch + 1
      val answer = lobby.requestExclusion(tableId, epoch)
      if !lastAnswer.contains(answer) then
        lastAnswer = Some(answer)
        System.out.println(s"[table $tableId] going on without the backup (epoch $epoch): $answer")
      answer match
        case PromotionAnswer.Granted     => table.goOnAlone(epoch)
        case PromotionAnswer.Denied(409) =>
          table.stepDown()
          onReplaced()
        case _ => () // no majority yet: asked again at the next period
