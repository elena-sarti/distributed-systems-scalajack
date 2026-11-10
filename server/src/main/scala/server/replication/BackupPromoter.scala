package server.replication

import server.liveness.{FailureDetector, Peer}
import server.{PromotionAnswer, PromotionGateway}

/** What a backup does when it stops hearing its primary. At every `tick` it asks the lobby for the promotion until the
 * lobby agrees, then it takes the place of the primary and does nothing more.
 *
 * @param takeOver starts the table in place of the primary, from the last state received (`None` if there is none) and
 *                  with the new epoch.
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
