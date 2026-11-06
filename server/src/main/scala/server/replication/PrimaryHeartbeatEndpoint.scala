package server.replication

import protocol.LobbyApi
import protocol.LobbyApi.ApiError
import server.auth.JsonPostEndpoint
import server.liveness.{FailureDetector, Peer}

/** `POST /replica/heartbeat` on the backup: the primary says it is alive. Answers 200 when it is the primary the
 * backup follows (a sign of life for the detector), 409 when it has been replaced, 400 when the body is not a
 * heartbeat. The backup's answer is in turn the proof, for the primary, that the backup is alive.
 *
 * @param detector the failure detector of the backup node, where the primary is watched.
 */
final class PrimaryHeartbeatEndpoint(backup: BackupReplica, detector: FailureDetector[Peer])
  extends JsonPostEndpoint("Primary heartbeat"):

  protected def answer(body: String): (Int, String) =
    LobbyApi.decode[ReplicaHeartbeat](body) match
      case Left(message) => (400, json(ApiError(message)))
      case Right(beat) if backup.follows(beat.epoch) =>
        detector.heardFrom(Peer.Primary)
        (200, json(HeartbeatAck(beat.epoch)))
      case Right(_) => (409, json(ApiError(ReplicaEndpoint.Superseded)))

object PrimaryHeartbeatEndpoint:
  /** A path of its own: the server picks the longest matching one, so it does not clash with `/replica`. */
  val Path = "/replica/heartbeat"
