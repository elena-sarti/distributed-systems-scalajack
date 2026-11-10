package server.liveness

import server.{HeartbeatResult, LobbyGateway}

/** A [[LobbyGateway]] that tells the detector whenever the lobby answers, since every answer proves that the lobby is
 * reachable. */
final class ObservedLobbyGateway(inner: LobbyGateway, detector: FailureDetector[Peer]) extends LobbyGateway:

  def register(id: String, host: String, wsPort: Int): Boolean =
    val accepted = inner.register(id, host, wsPort)
    if accepted then detector.heardFrom(Peer.Lobby)
    accepted // a refusal does not tell whether the lobby answered or not, so it proves nothing

  def heartbeat(id: String, freeSeats: Int): HeartbeatResult = observed(inner.heartbeat(id, freeSeats))

  def backupHeartbeat(tableId: String): HeartbeatResult = observed(inner.backupHeartbeat(tableId))

  /** Only a missing answer leaves the lobby unproven, since "unknown table" is still an answer. */
  private def observed(result: HeartbeatResult): HeartbeatResult =
    if result != HeartbeatResult.Unreachable then detector.heardFrom(Peer.Lobby)
    result
