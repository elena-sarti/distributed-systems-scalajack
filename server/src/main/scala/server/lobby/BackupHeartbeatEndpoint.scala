package server.lobby

import protocol.LobbyApi
import protocol.LobbyApi.{ApiError, BackupHeartbeat, TableAck}
import server.auth.JsonPostEndpoint
import server.liveness.FailureDetector

/** `POST /replicas/heartbeat`: the backup of a table says it is alive, which is a sign of life for the failure detector of
 * the lobby. The backups are not in the registry since they take no players.
 *
 * @param backups the failure detector of the lobby, where each backup is identified by the id of its table.
 */
final class BackupHeartbeatEndpoint(backups: FailureDetector[String]) extends JsonPostEndpoint("Backup heartbeat"):

  protected def answer(body: String): (Int, String) =
    LobbyApi.decode[BackupHeartbeat](body) match
      case Left(message)                       => (400, json(ApiError(message)))
      case Right(beat) if beat.tableId.isBlank => (400, json(ApiError("The id of the table is blank")))
      case Right(beat)                         =>
        backups.heardFrom(beat.tableId) // the first heartbeat is also what makes the lobby start watching it
        (200, json(TableAck(beat.tableId)))
