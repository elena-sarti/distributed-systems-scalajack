package server.lobby

import protocol.LobbyApi
import protocol.LobbyApi.{ApiError, BackupHeartbeat, TableAck}
import server.auth.JsonPostEndpoint
import server.liveness.FailureDetector

/** `POST /replicas/heartbeat`: the backup of a table says it is alive. Every heartbeat is a sign of life for the
 * failure detector of the lobby, which signals the backups that go silent (the lobby is the arbiter of the
 * decision record, so it must know whether a backup is reachable). The primaries are already watched by the
 * registry (E2).
 *
 * The backups are not in the table registry on purpose: they take no players, so they must never be assigned.
 *
 * @param backups the detector of the lobby, with the backups identified by the id of their table.
 */
final class BackupHeartbeatEndpoint(backups: FailureDetector[String]) extends JsonPostEndpoint("Backup heartbeat"):

  protected def answer(body: String): (Int, String) =
    LobbyApi.decode[BackupHeartbeat](body) match
      case Left(message)                   => (400, json(ApiError(message)))
      case Right(beat) if beat.tableId.isBlank => (400, json(ApiError("The id of the table is blank")))
      case Right(beat) =>
        backups.heardFrom(beat.tableId) // the first heartbeat is also what makes the lobby start watching it
        (200, json(TableAck(beat.tableId)))
