package server.liveness

import java.time.{Duration, Instant}

/** Whether a watched peer has given a sign of life recently. */
enum PeerStatus:
  case Alive, Suspected

/** Who is told when the detector changes its mind about a peer. Called outside the detector's lock, so a
 * listener may be slow or may call the detector back. F4 will plug the promotion of the backup in here. */
trait FailureListener[P]:
  def onSuspected(peer: P): Unit
  def onRecovered(peer: P): Unit

/** Failure detector based on heartbeats and a timeout: the same rule the lobby applies to the tables (E2),
 * written once and used by every node (DRY).
 *
 * A peer is *suspected* when nothing has been heard from it for longer than `timeout`; it is *alive* again as
 * soon as something is heard. Each change is reported once to the listener, not at every check.
 * The detector only decides; how often to check and what counts as a sign of life is up to the caller.
 *
 * Thread-safe: signs of life arrive from HTTP threads while the periodic task checks.
 *
 * @param timeout how long a peer may stay silent (a few heartbeat periods, so one lost heartbeat is not enough).
 * @param clock   the current time; the tests replace it, so the rules are deterministic.
 */
final class FailureDetector[P](timeout: Duration, listener: FailureListener[P], clock: () => Instant = Instant.now):
  private final case class Seen(at: Instant, status: PeerStatus)

  private var peers = Map.empty[P, Seen]

  /** Starts watching a peer as if it had just been heard: it gets a full timeout to show up. Does nothing
   * if the peer is already watched. */
  def watch(peer: P): Unit = synchronized:
    if !peers.contains(peer) then peers = peers.updated(peer, Seen(clock(), PeerStatus.Alive))

  /** A sign of life (also from a peer not watched yet, which starts being watched). */
  def heardFrom(peer: P): Unit =
    val wasSuspected = synchronized:
      val previous = peers.get(peer)
      peers = peers.updated(peer, Seen(clock(), PeerStatus.Alive))
      previous.exists(_.status == PeerStatus.Suspected)
    if wasSuspected then listener.onRecovered(peer)

  /** Suspects the alive peers silent for longer than the timeout (a peer heard exactly `timeout` ago is
   * still alive).
   * @return the peers that have just become suspected by this check. */
  def check(): Seq[P] =
    val newlySuspected = synchronized:
      val limit  = clock().minus(timeout)
      val silent = peers.collect { case (peer, seen) if seen.status == PeerStatus.Alive && seen.at.isBefore(limit) => peer }.toSeq
      peers = peers ++ silent.map(peer => peer -> peers(peer).copy(status = PeerStatus.Suspected))
      silent
    newlySuspected.foreach(listener.onSuspected)
    newlySuspected

  /** Stops watching a peer, without any signal (e.g. a backup that has become the primary). */
  def forget(peer: P): Unit = synchronized:
    peers = peers - peer

  /** `None` if the peer is not watched. */
  def status(peer: P): Option[PeerStatus] = synchronized(peers.get(peer).map(_.status))

  def isSuspected(peer: P): Boolean = status(peer).contains(PeerStatus.Suspected)

/** The listener that signals the failures in the log.
 *
 * @param observer who is watching, to tell the nodes' logs apart (e.g. `table table-1`).
 * @param describe how a peer is named in the message.
 */
final class LoggingFailureListener[P](observer: String, describe: P => String = (peer: P) => peer.toString) extends FailureListener[P]:
  def onSuspected(peer: P): Unit =
    System.out.println(s"[$observer] ${describe(peer)} is unreachable: no sign of life within the timeout")

  def onRecovered(peer: P): Unit =
    System.out.println(s"[$observer] ${describe(peer)} is reachable again")
