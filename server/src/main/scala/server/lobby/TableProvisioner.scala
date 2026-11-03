package server.lobby

/** How the lobby gets one more table node running. A trait because starting a node depends on where the
 * system runs (Docker, another orchestrator, a test): the dispatcher only needs to ask. */
trait TableProvisioner:
  /** Asks for the node of the table `tableId` to be started. The node is not ready when this returns: it
   * is ready when it registers at the lobby by itself. May take a while (it talks to the outside).
   *
   * @return `true` if the request was accepted (or the node was already running), `false` if it failed.
   *         Never throws. */
  def start(tableId: String): Boolean
