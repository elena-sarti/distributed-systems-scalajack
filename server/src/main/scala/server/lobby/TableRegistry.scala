package server.lobby

import utils.ModelExports.Game

/** The registry of the tables, kept by the lobby: which tables exist, where they are, how many seats are
 * free and whether they can be used. It is the only place where the lobby keeps this information, so the
 * services built on it (heartbeats, dispatching) all see the same state.
 *
 * It lives in memory on purpose: it describes the nodes that are running now, and it is rebuilt when they
 * register again, so persisting it would only keep stale data after a restart.
 *
 * Thread-safe: it is used by several threads at once (every HTTP request of the lobby is served by a
 * worker). Every operation is atomic on the whole registry.
 *
 * @param capacity how many players every table can seat.
 */
final class TableRegistry(capacity: Int = Game.MaxPlayersNum):
  private var byId = Map.empty[String, TableInfo]

  /** Adds a table, with every seat free and available. If the id is already there (e.g. the node was
   * restarted) the old entry is replaced: a restarted table starts empty, and its old state would be wrong. */
  def register(id: String, host: String, wsPort: Int): Either[RegistryError, TableInfo] =
    validate(id, host, wsPort).toLeft(TableInfo(id, host, wsPort, capacity, capacity, TableStatus.Available)).map: info =>
      synchronized:
        byId = byId.updated(id, info)
      info

  /** Sets how many seats are free, as reported by the table. Refuses a number outside 0 to the capacity. */
  def updateFreeSeats(id: String, free: Int): Either[RegistryError, TableInfo] =
    if free < 0 || free > capacity then Left(RegistryError.InvalidFreeSeats(free, capacity))
    else change(id)(_.copy(freeSeats = free))

  /** Marks a table as usable or not, leaving its seats as they are. */
  def markStatus(id: String, status: TableStatus): Either[RegistryError, TableInfo] =
    change(id)(_.copy(status = status))

  /** Forgets a table. @return `true` if it was there. */
  def remove(id: String): Boolean = synchronized:
    val known = byId.contains(id)
    byId = byId - id
    known

  def find(id: String): Option[TableInfo] = synchronized(byId.get(id))

  /** A snapshot of all the tables, ordered by id so that the result is always the same for the same state. */
  def tables: Seq[TableInfo] = synchronized(byId.values.toSeq).sortBy(_.id)

  /** Applies a change to a known table. Single place for "find, change, store" (DRY). */
  private def change(id: String)(update: TableInfo => TableInfo): Either[RegistryError, TableInfo] = synchronized:
    byId.get(id).map(update).toRight(RegistryError.UnknownTable(id)).map: changed =>
      byId = byId.updated(id, changed)
      changed

  private def validate(id: String, host: String, wsPort: Int): Option[RegistryError] =
    if id.isBlank then Some(RegistryError.InvalidTable("The id of the table is blank"))
    else if host.isBlank then Some(RegistryError.InvalidTable("The host of the table is blank"))
    else if wsPort < 1 || wsPort > 65535 then Some(RegistryError.InvalidTable(s"Invalid port $wsPort"))
    else None
