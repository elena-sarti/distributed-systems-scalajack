package server.lobby

import protocol.LobbyApi
import protocol.LobbyApi.{ApiError, ExclusionGranted, ExclusionRequest}
import server.auth.JsonPostEndpoint

/** `POST /tables/exclude-backup`: the primary of a table, which cannot reach its backup, asks to go on without it.
 * The decision is the arbiter's; this only turns it into an HTTP answer: 200 (granted), 409 (the epoch is not newer:
 * this primary has been replaced), 404 (unknown table). */
final class BackupExclusionEndpoint(arbiter: PromotionArbiter) extends JsonPostEndpoint("Backup exclusion"):

  protected def answer(body: String): (Int, String) =
    LobbyApi.decode[ExclusionRequest](body) match
      case Left(message)  => (400, json(ApiError(message)))
      case Right(request) => reply(request, arbiter.exclude(request.tableId, request.epoch))

  private def reply(request: ExclusionRequest, decision: PromotionDecision): (Int, String) = decision match
    case PromotionDecision.Granted(epoch) =>
      System.out.println(s"Table ${request.tableId}: its primary goes on without the backup, epoch $epoch")
      (200, json(ExclusionGranted(request.tableId, epoch)))
    case PromotionDecision.StaleEpoch(current) =>
      (409, json(ApiError(s"Epoch ${request.epoch} is not newer than $current")))
    case PromotionDecision.UnknownTable =>
      (404, json(ApiError(s"Unknown table '${request.tableId}'")))
    case PromotionDecision.PrimaryAlive => // never decided for an exclusion: the one asking is the primary
      (409, json(ApiError(s"The configuration of table '${request.tableId}' cannot change now")))
