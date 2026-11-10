package server.lobby

import protocol.LobbyApi
import protocol.LobbyApi.{ApiError, PromotionGranted, PromotionRequest}
import server.auth.JsonPostEndpoint

/** `POST /tables/promote`: the backup of a table asks to become its primary.
 * 200 if granted, 409 if the lobby does not agree, 404 if the table is unknown. */
final class PromotionEndpoint(arbiter: PromotionArbiter) extends JsonPostEndpoint("Promotion"):

  protected def answer(body: String): (Int, String) =
    LobbyApi.decode[PromotionRequest](body) match
      case Left(message)  => (400, json(ApiError(message)))
      case Right(request) => reply(request, arbiter.decide(request.tableId, request.epoch))

  private def reply(request: PromotionRequest, decision: PromotionDecision): (Int, String) = decision match
    case PromotionDecision.Granted(epoch)      =>
      System.out.println(s"Table ${request.tableId}: its backup is promoted to primary, epoch $epoch")
      (200, json(PromotionGranted(request.tableId, epoch)))
    case PromotionDecision.PrimaryAlive        =>
      (409, json(ApiError(s"The lobby still hears the primary of table '${request.tableId}'")))
    case PromotionDecision.StaleEpoch(current) =>
      (409, json(ApiError(s"Epoch ${request.epoch} is not newer than $current")))
    case PromotionDecision.UnknownTable        =>
      (404, json(ApiError(s"Unknown table '${request.tableId}'")))
