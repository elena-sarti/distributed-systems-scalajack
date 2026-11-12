package server.lobby

import utils.ModelExports.Game
import java.time.{Duration, Instant}

/** The registry of the tables kept by the lobby, with their address, free seats and status. It also remembers when each
 * table was last heard from and which seats are promised to players not seated yet. It lives in
 * memory and is thread-safe, with every operation atomic on the whole registry.
 *
 * @param capacity how many players every table can seat.
 */
final class TableRegistry(capacity: Int = Game.MaxPlayersNum):
  /** A table, the last time it was heard from and the seats promised to players who may not have sat down yet, with one
   * expiry instant for each. */
  private final case class Entry(info: TableInfo, lastSeen: Instant, reservations: Vector[Instant] = Vector.empty):
    /** The seats that can still be promised at `at`, which are the free ones less the promises not yet expired. The table
     * reports its free seats only from time to time, so alone they could be given twice. */
    def assignable(at: Instant): Int = math.max(0, info.freeSeats - reservations.count(_.isAfter(at)))

  private var byId = Map.empty[String, Entry]

  /** Adds a table with every seat free and available. An entry with the same id is replaced, since a restarted table starts
   * empty. */
  def register(id: String, host: String, wsPort: Int, at: Instant = Instant.now()): Either[RegistryError, TableInfo] =
    validate(id, host, wsPort).toLeft(TableInfo(id, host, wsPort, capacity, capacity, TableStatus.Available)).map: info =>
      synchronized:
        byId = byId.updated(id, Entry(info, at))
      info

  /** Sets how many seats are free, as reported by the table. Refuses a number outside 0 to the capacity. */
  def updateFreeSeats(id: String, free: Int): Either[RegistryError, TableInfo] =
    withSeats(id, free, None)(identity)

  /** Records a sign of life from a table heard at `at`, with its free seats. The table becomes available, also if it had
   * been marked unavailable for being silent. */
  def heartbeat(id: String, free: Int, at: Instant = Instant.now()): Either[RegistryError, TableInfo] =
    withSeats(id, free, Some(at))(entry => entry.copy(info = entry.info.copy(status = TableStatus.Available), lastSeen = at))

  /** Marks a table as usable or not, leaving its seats as they are. */
  def markStatus(id: String, status: TableStatus): Either[RegistryError, TableInfo] =
    change(id)(entry => entry.copy(info = entry.info.copy(status = status)))

  /** Marks as unavailable the available tables last heard from before `lastSeenBefore`. A table heard exactly at that
   * instant is still alive.
   *
   * @return the tables just marked, so the caller can tell that they went silent.
   */
  def markSilentUnavailable(lastSeenBefore: Instant): Seq[TableInfo] = synchronized:
    val silent = byId.values.filter(entry => entry.info.status == TableStatus.Available && entry.lastSeen.isBefore(lastSeenBefore))
    val marked = silent.map(entry => entry.copy(info = entry.info.copy(status = TableStatus.Unavailable))).toSeq
    byId = byId ++ marked.map(entry => entry.info.id -> entry)
    marked.map(_.info).sortBy(_.id)

  /** Promises a seat at the first available table by id and keeps it taken for `holdFor`. Choice and reservation are
   * atomic, so two players never get the same seat.
   *
   * @return the table with its address, or `None` if no seat is left.
   */
  def reserveSeat(at: Instant, holdFor: Duration): Option[TableInfo] = synchronized:
    val candidate = byId.values.toSeq.sortBy(_.info.id)
      .find(entry => entry.info.status == TableStatus.Available && entry.assignable(at) > 0)
    candidate.map: entry =>
      // expired reservations are dropped now, so the list does not grow
      val reserved = entry.copy(reservations = entry.reservations.filter(_.isAfter(at)) :+ at.plus(holdFor))
      byId = byId.updated(entry.info.id, reserved)
      entry.info

  /** How many seats of a table can still be promised at `at`, or None if the table is not known. */
  def assignableSeats(id: String, at: Instant): Option[Int] = synchronized(byId.get(id).map(_.assignable(at)))

  /** Forgets a table.
   *
   * @return true if it was there.
   */
  def remove(id: String): Boolean = synchronized:
    val known = byId.contains(id)
    byId = byId - id
    known

  def find(id: String): Option[TableInfo] = synchronized(byId.get(id).map(_.info))

  /** A snapshot of all the tables, ordered by id. */
  def tables: Seq[TableInfo] = synchronized(byId.values.map(_.info).toSeq).sortBy(_.id)

  /** Checks and sets the free seats of a known table, then applies `adjust` to the entry. Seats taken since the last report
   * are the reserved players who have sat down: as many reservations as seats taken are used up, the oldest first, so
   * that a seated player is not counted twice, once by the table and once by the reservation. */
  private def withSeats(id: String, free: Int, at: Option[Instant])(adjust: Entry => Entry): Either[RegistryError, TableInfo] =
    if free < 0 || free > capacity then Left(RegistryError.InvalidFreeSeats(free, capacity))
    else change(id): entry =>
      val taken = math.max(0, entry.info.freeSeats - free)
      val live  = at.fold(entry.reservations)(now => entry.reservations.filter(_.isAfter(now)))
      adjust(entry.copy(info = entry.info.copy(freeSeats = free), reservations = live.drop(taken)))

  /** Applies a change to a known table. Finding, changing and storing are done only here. */
  private def change(id: String)(update: Entry => Entry): Either[RegistryError, TableInfo] = synchronized:
    byId.get(id).map(update).toRight(RegistryError.UnknownTable(id)).map: changed =>
      byId = byId.updated(id, changed)
      changed.info

  private def validate(id: String, host: String, wsPort: Int): Option[RegistryError] =
    if id.isBlank then Some(RegistryError.InvalidTable("The id of the table is blank"))
    else if host.isBlank then Some(RegistryError.InvalidTable("The host of the table is blank"))
    else if wsPort < 1 || wsPort > 65535 then Some(RegistryError.InvalidTable(s"Invalid port $wsPort"))
    else None
