package server.lobby

import utils.ModelExports.Game
import java.time.Instant

/** The registry of the tables, kept by the lobby: which tables exist, where they are, how many seats are
 * free and whether they can be used. It is the only place where the lobby keeps this information, so the
 * services built on it (heartbeats, dispatching) all see the same state.
 *
 * It also remembers when each table was last heard from, so that a table that goes silent can be found.
 * The time is always given by the caller (never read here when it matters), so the rules are deterministic.
 *
 * It lives in memory on purpose: it describes the nodes that are running now, and it is rebuilt when they
 * register again, so persisting it would only keep stale data after a restart.
 *
 * Thread-safe: it is used by several threads at once (every HTTP request of the lobby is served by a
 * worker, and the heartbeat monitor runs on its own). Every operation is atomic on the whole registry.
 *
 * @param capacity how many players every table can seat.
 */
final class TableRegistry(capacity: Int = Game.MaxPlayersNum):
  /** A table together with the last time it was heard from (registration or heartbeat). */
  private final case class Entry(info: TableInfo, lastSeen: Instant)

  private var byId = Map.empty[String, Entry]

  /** Adds a table, with every seat free and available. If the id is already there (e.g. the node was
   * restarted) the old entry is replaced: a restarted table starts empty, and its old state would be wrong. */
  def register(id: String, host: String, wsPort: Int, at: Instant = Instant.now()): Either[RegistryError, TableInfo] =
    validate(id, host, wsPort).toLeft(TableInfo(id, host, wsPort, capacity, capacity, TableStatus.Available)).map: info =>
      synchronized:
        byId = byId.updated(id, Entry(info, at))
      info

  /** Sets how many seats are free, as reported by the table. Refuses a number outside 0 to the capacity. */
  def updateFreeSeats(id: String, free: Int): Either[RegistryError, TableInfo] =
    withSeats(id, free)(identity)

  /** A sign of life from a table: it tells how many seats are free, it is heard at `at`, and it is
   * available (also when it had been marked unavailable because it was silent). */
  def heartbeat(id: String, free: Int, at: Instant = Instant.now()): Either[RegistryError, TableInfo] =
    withSeats(id, free)(entry => Entry(entry.info.copy(status = TableStatus.Available), at))

  /** Marks a table as usable or not, leaving its seats as they are. */
  def markStatus(id: String, status: TableStatus): Either[RegistryError, TableInfo] =
    change(id)(entry => entry.copy(info = entry.info.copy(status = status)))

  /** Marks as unavailable the available tables last heard from before `lastSeenBefore` (a table heard
   * exactly at that instant is still alive).
   * @return the tables that have just been marked, so the caller can tell that they went silent. */
  def markSilentUnavailable(lastSeenBefore: Instant): Seq[TableInfo] = synchronized:
    val silent = byId.values.filter(entry => entry.info.status == TableStatus.Available && entry.lastSeen.isBefore(lastSeenBefore))
    val marked = silent.map(entry => entry.copy(info = entry.info.copy(status = TableStatus.Unavailable))).toSeq
    byId = byId ++ marked.map(entry => entry.info.id -> entry)
    marked.map(_.info).sortBy(_.id)

  /** Forgets a table. @return `true` if it was there. */
  def remove(id: String): Boolean = synchronized:
    val known = byId.contains(id)
    byId = byId - id
    known

  def find(id: String): Option[TableInfo] = synchronized(byId.get(id).map(_.info))

  /** A snapshot of all the tables, ordered by id so that the result is always the same for the same state. */
  def tables: Seq[TableInfo] = synchronized(byId.values.map(_.info).toSeq).sortBy(_.id)

  /** Sets the free seats of a known table (after checking them), then applies `adjust` to the entry. */
  private def withSeats(id: String, free: Int)(adjust: Entry => Entry): Either[RegistryError, TableInfo] =
    if free < 0 || free > capacity then Left(RegistryError.InvalidFreeSeats(free, capacity))
    else change(id)(entry => adjust(entry.copy(info = entry.info.copy(freeSeats = free))))

  /** Applies a change to a known table. Single place for "find, change, store" (DRY). */
  private def change(id: String)(update: Entry => Entry): Either[RegistryError, TableInfo] = synchronized:
    byId.get(id).map(update).toRight(RegistryError.UnknownTable(id)).map: changed =>
      byId = byId.updated(id, changed)
      changed.info

  private def validate(id: String, host: String, wsPort: Int): Option[RegistryError] =
    if id.isBlank then Some(RegistryError.InvalidTable("The id of the table is blank"))
    else if host.isBlank then Some(RegistryError.InvalidTable("The host of the table is blank"))
    else if wsPort < 1 || wsPort > 65535 then Some(RegistryError.InvalidTable(s"Invalid port $wsPort"))
    else None
