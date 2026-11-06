package server.liveness

import server.{HeartbeatResult, LobbyGateway}

/** A [[LobbyGateway]] that also tells the detector when the lobby answers: every answer, whatever it says, proves
 * that the lobby is reachable. It is a decorator, so the announcer and the transport stay as they are (OCP). */
final class ObservedLobbyGateway(inner: LobbyGateway, detector: FailureDetector[Peer]) extends LobbyGateway:

  def register(id: String, host: String, wsPort: Int): Boolean =
    val accepted = inner.register(id, host, wsPort)
    if accepted then detector.heardFrom(Peer.Lobby)
    accepted // a refusal does not tell whether the lobby answered or not, so it proves nothing

  def heartbeat(id: String, freeSeats: Int): HeartbeatResult = observed(inner.heartbeat(id, freeSeats))

  def backupHeartbeat(tableId: String): HeartbeatResult = observed(inner.backupHeartbeat(tableId))

  /** Only "no answer" leaves the lobby unproven: "unknown table" is still an answer. */
  private def observed(result: HeartbeatResult): HeartbeatResult =
    if result != HeartbeatResult.Unreachable then detector.heardFrom(Peer.Lobby)
    result
