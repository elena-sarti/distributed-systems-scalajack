package server.lobby

/** How the lobby gets one more table node running. A trait since starting a node depends on where the system runs, and
 * the dispatcher only needs to ask. */
trait TableProvisioner:
  /** Asks for the node of the table `tableId` to be started. The node is ready only when it registers at the lobby by
   * itself. May take a while.
   *
   * @return `true` if the request was accepted or the node was already running, `false` if it failed. Never throws.
   */
  def start(tableId: String): Boolean

object TableProvisioner:
  /** Whether the id is made of letters, digits, `_`, `.` and `-`, and does not start with a symbol. Such an id is a valid
   * container name and cannot alter the path of a request. */
  def isValidId(id: String): Boolean = id.matches("[A-Za-z0-9][A-Za-z0-9_.-]*")

