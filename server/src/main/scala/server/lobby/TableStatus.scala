package server.lobby

/** Whether the lobby can send players to a table. */
enum TableStatus:
  /** The table answers: players can be sent there. */
  case Available
  /** The table is not reachable (e.g. no heartbeat in time): nobody is sent there. */
  case Unavailable

/** What the lobby knows about one table node.
 *
 * @param id       the name of the table, unique in the registry.
 * @param host     where the node can be reached by the clients.
 * @param wsPort   the WebSocket port of the node: the client connects there directly after the assignment.
 * @param capacity how many players the table can seat.
 * @param freeSeats how many of them are free right now (0 to `capacity`).
 */
final case class TableInfo(id: String, host: String, wsPort: Int, capacity: Int, freeSeats: Int, status: TableStatus):
  /** A player can be sent here only if the table is reachable and not full. */
  def isJoinable: Boolean = status == TableStatus.Available && freeSeats > 0