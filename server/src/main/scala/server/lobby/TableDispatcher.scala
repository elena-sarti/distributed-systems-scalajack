package server.lobby

import java.time.{Duration, Instant}

/** What happens to a player who asks for a seat. */
enum Assignment:
  /** The seat is promised: the player connects to this table (address in the table). */
  case Assigned(table: TableInfo)
  /** No seat now, but the node of this table is being started: the player should ask again shortly. */
  case Starting(tableId: String)
  /** No seat and no more tables can be started: the server is full. */
  case Full
  /** No seat, and the node of this table could not be started. */
  case ActivationFailed(tableId: String)

/** Decides to which table each player goes: the first with a free seat; if there is none, it starts a new
 * node, as long as the maximum number of tables is not reached; beyond that, "full".
 *
 * Every request is handled one at a time (`synchronized`): the lobby has few requests, and this way
 * "look for a seat, else start a node" cannot be done twice for the same need. The seats themselves are
 * promised by the registry, which is atomic also against the heartbeats that arrive meanwhile.
 *
 * @param pool         the ids of the tables that can exist, in the order they are started. Its size is the
 *                     maximum number of tables: nothing else is ever started.
 * @param seatHold     how long a promised seat stays taken if the player does not sit down.
 * @param startTimeout how long a node that was asked to start may take to register, before it is
 *                     considered lost and its start can be asked again.
 * @param clock        the current time; the tests replace it.
 */
final class TableDispatcher(registry: TableRegistry, provisioner: TableProvisioner, pool: Seq[String],
                            seatHold: Duration = TableDispatcher.DefaultSeatHold,
                            startTimeout: Duration = TableDispatcher.DefaultStartTimeout,
                            clock: () => Instant = Instant.now):

  /** The nodes asked to start that have not registered yet, and when they were asked. */
  private var starting = Map.empty[String, Instant]

  def assign(): Assignment = synchronized:
    val now = clock()
    forgetArrivedOrLost(now)
    registry.reserveSeat(now, seatHold).map(Assignment.Assigned(_)).getOrElse(startNode(now))

  /** A node that has registered is no longer starting (its seats are now in the registry); one that took
   * too long is forgotten, so that it can be asked again instead of blocking every new table. */
  private def forgetArrivedOrLost(now: Instant): Unit =
    starting = starting.filter((id, since) => registry.find(id).isEmpty && since.plus(startTimeout).isAfter(now))

  private def startNode(now: Instant): Assignment =
    starting.keys.minOption match
      case Some(id) => Assignment.Starting(id) // one is already on its way: one is enough for now
      case None =>
        // a table that is registered counts even if it is unavailable: its node exists, so it is not started again
        pool.find(id => registry.find(id).isEmpty) match
          case None     => Assignment.Full
          case Some(id) =>
            if provisioner.start(id) then
              starting = starting.updated(id, now)
              Assignment.Starting(id)
            else Assignment.ActivationFailed(id)

object TableDispatcher:
  /** Long enough for a client to connect after being assigned, short enough not to lose a seat for long. */
  val DefaultSeatHold: Duration = Duration.ofSeconds(10)
  /** A node (a container) needs some seconds to start and to register. */
  val DefaultStartTimeout: Duration = Duration.ofSeconds(30)
