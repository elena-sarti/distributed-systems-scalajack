package server

/** What a table node learns from a heartbeat. */
enum HeartbeatResult:
  /** The lobby has taken it. */
  case Acknowledged
  /** The lobby does not know this table, so it must register again. */
  case UnknownTable
  /** No answer, so the node can only try again later. */
  case Unreachable
  /** The lobby refuses this node: another node now plays the table with a newer epoch. */
  case Superseded

/** How a table node talks to the lobby, so that the announcer does not depend on the transport. */
trait LobbyGateway:
  /** Registers the table at the lobby.
   *
   * @return `true` if the lobby accepted the table.
   */
  def register(id: String, host: String, wsPort: Int): Boolean

  /** Sends a heartbeat with the free seats of the table. */
  def heartbeat(id: String, freeSeats: Int): HeartbeatResult

  /** The backup of table `tableId` tells the lobby it is alive. */
  def backupHeartbeat(tableId: String): HeartbeatResult
