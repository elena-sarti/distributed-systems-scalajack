package server.lobby

/** Whether the lobby can send players to a table. */
enum TableStatus:
  /** The table answers, so players can be sent there. */
  case Available
  /** The table is not reachable, so nobody is sent there. */
  case Unavailable

/** What the lobby knows about one table node.
 *
 * @param id        the name of the table, unique in the registry.
 * @param host      where the clients can reach the node.
 * @param wsPort    the WebSocket port of the node, where the client connects directly after the assignment.
 * @param capacity  how many players the table can seat.
 * @param freeSeats how many seats are free right now, from 0 to `capacity`.
 * @param status    whether the lobby can send players to the table.
 */
final case class TableInfo(id: String, host: String, wsPort: Int, capacity: Int, freeSeats: Int, status: TableStatus):
  /** A player can be sent here only if the table is reachable and not full. */
  def isJoinable: Boolean = status == TableStatus.Available && freeSeats > 0