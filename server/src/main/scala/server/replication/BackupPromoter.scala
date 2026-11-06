package server.replication

import server.liveness.{FailureDetector, Peer}
import server.{PromotionAnswer, PromotionGateway}

/** What a backup does when it stops hearing its primary: asks the lobby for the promotion, and if the lobby agrees
 * (two of three: the decision record) takes the place of the primary.
 *
 * `tick` is called at every period by the liveness task, so the request is repeated for as long as the primary is
 * suspected and the lobby has not agreed: the lobby may notice the silence a moment after the backup, or may be
 * unreachable for a while. Once promoted it does nothing more.
 *
 * @param takeOver starts the table in place of the primary, from the last state received (`None` if the primary
 *                 never sent one) and with the new epoch. It runs once.
 */
final class BackupPromoter(tableId: String, replica: BackupReplica, peers: FailureDetector[Peer],
                           lobby: PromotionGateway, takeOver: (Option[TableSnapshot], Long) => Unit):
  private var promoted = false
  private var lastAnswer: Option[PromotionAnswer] = None // to log only when the answer changes, not at every period

  def isPromoted: Boolean = synchronized(promoted)

  def tick(): Unit = synchronized:
    if !promoted && peers.isSuspected(Peer.Primary) then attempt()

  private def attempt(): Unit =
    val latest = replica.latest
    // greater than the epoch of the primary that failed: whatever it still sends is refused (see `BackupReplica`)
    val epoch  = latest.map(_.epoch).getOrElse(0L) + 1
    val answer = lobby.requestPromotion(tableId, epoch)
    if !lastAnswer.contains(answer) then
      lastAnswer = Some(answer)
      System.out.println(s"[backup $tableId] promotion to primary (epoch $epoch): $answer")
    if answer == PromotionAnswer.Granted then
      promoted = true
      replica.close() // from now on it follows nobody
      takeOver(latest.map(_.snapshot), epoch)
