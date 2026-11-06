package server

/** What a table node learns from a heartbeat. */
enum HeartbeatResult:
  /** The lobby has taken it. */
  case Acknowledged
  /** The lobby answered, but does not know this table (e.g. it was restarted): register again. */
  case UnknownTable
  /** No answer: the lobby is down or the network is. Nothing to do but try again later. */
  case Unreachable
  /** The lobby refuses this node: another node now plays the table with a newer epoch. */
  case Superseded

/** How a table node talks to the lobby. A trait so that the announcer can be tested without a network
 * and the transport can change without touching it. */
trait LobbyGateway:
  /** @return `true` if the lobby accepted the table. Never throws. */
  def register(id: String, host: String, wsPort: Int): Boolean

  /** Never throws. */
  def heartbeat(id: String, freeSeats: Int): HeartbeatResult

  /** The backup of table `tableId` tells the lobby it is alive. Never throws. */
  def backupHeartbeat(tableId: String): HeartbeatResult
