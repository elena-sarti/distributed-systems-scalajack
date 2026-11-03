package server.replication

import protocol.LobbyApi
import protocol.LobbyApi.ApiError
import server.auth.JsonPostEndpoint

/** `POST /replica` on the backup: the primary sends a [[ReplicaUpdate]] and gets 200 when it is stored,
 * 409 when the backup already follows a newer primary, 400 when the body is not an update. */
final class ReplicaEndpoint(backup: BackupReplica)
  extends JsonPostEndpoint("Replication", ReplicaEndpoint.MaxBodyBytes):

  protected def answer(body: String): (Int, String) =
    LobbyApi.decode[ReplicaUpdate](body) match
      case Left(message) => (400, json(ApiError(message)))
      case Right(update) =>
        if backup.store(update) then (200, json(ReplicaAck(update.epoch, update.sequence)))
        else (409, json(ApiError(ReplicaEndpoint.Superseded)))

object ReplicaEndpoint:
  val Path = "/replica"

  /** A snapshot holds the whole deck still to deal: some tens of kilobytes. */
  val MaxBodyBytes: Int = 1024 * 1024

  val Superseded = "This backup follows a newer primary"
