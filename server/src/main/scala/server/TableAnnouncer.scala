package server

/** Registers a table at the lobby and sends a heartbeat with the free seats at every tick.
 * If the lobby forgets the table it is registered again.
 * tick must be called by one thread at a time.
 *
 * @param host         the address the clients use to reach the table.
 * @param wsPort       the WebSocket port the clients use.
 * @param seats        where the number of free seats is read at each heartbeat.
 * @param onSuperseded called when the lobby says that another node plays this table now (see
 *                     [[HeartbeatResult.Superseded]]), so this one must step down.
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
        case HeartbeatResult.Superseded   =>
          System.out.println(s"Table $id is played by another node now: this one steps down")
          onSuperseded()
        case _                            => () // acknowledged or not reachable: the next heartbeat will tell
