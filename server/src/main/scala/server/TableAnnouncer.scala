package server

/** Keeps the lobby informed about a table: registers it, then sends a heartbeat with the free seats at
 * every `tick`. If the lobby forgets the table (it restarted) the table registers again, so the two
 * recover by themselves in any order of start-up.
 *
 * `tick` must be called by one thread at a time (see `PeriodicTask`); the class keeps no lock.
 *
 * @param host   the address the clients must use to reach the table, as the lobby will hand it out.
 * @param wsPort the WebSocket port clients must use (it may differ from the node's own, e.g. with Docker).
 * @param seats  where the number of free seats is read, at the moment of each heartbeat.
 * @param onSuperseded called when the lobby says that another node now plays this table (see
 *                     [[HeartbeatResult.Superseded]]): this one must step down.
 */
final class TableAnnouncer(gateway: LobbyGateway, id: String, host: String, wsPort: Int, seats: SeatAvailability,
                           onSuperseded: () => Unit = () => ()):
  private var registered = false

  def tick(): Unit =
    if !registered then
      registered = gateway.register(id, host, wsPort)
      if registered then System.out.println(s"Table $id registered at the lobby")
    if registered then
      gateway.heartbeat(id, seats.freeSeats) match
        case HeartbeatResult.UnknownTable =>
          System.out.println(s"The lobby does not know table $id any more: registering again")
          registered = false
        case HeartbeatResult.Superseded =>
          System.out.println(s"Table $id is played by another node now: this one steps down")
          onSuperseded()
        case _ => () // acknowledged, or not reachable: the next heartbeat will tell
