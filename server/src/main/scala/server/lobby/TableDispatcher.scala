package server.lobby

import java.time.{Duration, Instant}

/** What happens to a player who asks for a seat. */
enum Assignment:
  /** A seat is promised, so the player connects to this table. */
  case Assigned(table: TableInfo)
  /** No seat now but the node of this table is starting, so the player should ask again shortly. */
  case Starting(tableId: String)
  /** No seat and no more tables can be started. */
  case Full
  /** No seat, and the node of this table could not be started. */
  case ActivationFailed(tableId: String)

/** Decides to which table each player goes. It picks the first table with a free seat, otherwise it starts a new node if
 * the maximum is not reached, otherwise the server is full.
 * Decisions are taken one at a time.
 * Docker is called outside the lock, so the other requests are answered at once with "starting" and the heartbeats are not delayed.
 *
 * @param pool         the ids of the tables that can exist, in the order they are started. Its size is the maximum
 *                     number of tables.
 * @param seatHold     how long a promised seat stays taken if the player does not sit down.
 * @param startTimeout how long a node asked to start may take to register before it can be asked again.
 * @param clock        the current time.
 */
final class TableDispatcher(registry: TableRegistry, provisioner: TableProvisioner, pool: Seq[String],
                            seatHold: Duration = TableDispatcher.DefaultSeatHold,
                            startTimeout: Duration = TableDispatcher.DefaultStartTimeout,
                            clock: () => Instant = Instant.now):

  /** The nodes asked to start that have not registered yet, and when they were asked. */
  private var starting = Map.empty[String, Instant]

  /** Promises a seat to a player, or tells why it cannot. */
  def assign(): Assignment =
    decide() match
      case Left(answer)  => answer
      case Right(nodeId) => launch(nodeId)

  /** The answer, or the node that this request has to start. */
  private def decide(): Either[Assignment, String] = synchronized:
    val now = clock()
    forgetArrivedOrLost(now)
    registry.reserveSeat(now, seatHold) match
      case Some(table) => Left(Assignment.Assigned(table))
      case None        => nodeToStart(now)

  /** Asks for the node to be started, without holding the lock. */
  private def launch(nodeId: String): Assignment =
    if provisioner.start(nodeId) then Assignment.Starting(nodeId)
    else
      synchronized { starting = starting - nodeId } // not on its way after all: the next request tries again
      Assignment.ActivationFailed(nodeId)

  /** Forgets the nodes that have registered, whose seats are now in the registry, and those that took too long, so they can
   * be asked again instead of blocking every new table. */
  private def forgetArrivedOrLost(now: Instant): Unit =
    starting = starting.filter((id, since) => registry.find(id).isEmpty && since.plus(startTimeout).isAfter(now))

  private def nodeToStart(now: Instant): Either[Assignment, String] =
    starting.keys.minOption match
      case Some(id) => Left(Assignment.Starting(id)) // one is already on its way: one is enough for now
      case None =>
        // a table that is registered counts even if it is unavailable: its node exists, so it is not started again
        pool.find(id => registry.find(id).isEmpty) match
          case None     => Left(Assignment.Full)
          case Some(id) =>
            starting = starting.updated(id, now) // marked before Docker is asked: meanwhile nobody starts it again
            Right(id)

object TableDispatcher:
  /** Long enough for a client to connect after being assigned, short enough not to lose a seat for long. */
  val DefaultSeatHold: Duration = Duration.ofSeconds(10)
  /** A node (a container) needs some seconds to start and to register. */
  val DefaultStartTimeout: Duration = Duration.ofSeconds(30)
