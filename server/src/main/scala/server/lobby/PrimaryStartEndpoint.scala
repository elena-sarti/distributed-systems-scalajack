package server.lobby

import protocol.LobbyApi
import protocol.LobbyApi.{ApiError, StartGranted, StartRequest}
import server.auth.JsonPostEndpoint

/** `POST /tables/start`: a replicated table node that starts asks to be the primary of its table.
 *  Answers 200 with the epoch, or 409 while another node plays the table. */
final class PrimaryStartEndpoint(arbiter: PromotionArbiter) extends JsonPostEndpoint("Primary start"):

  protected def answer(body: String): (Int, String) =
    LobbyApi.decode[StartRequest](body) match
      case Left(message)                             => (400, json(ApiError(message)))
      case Right(request) if request.tableId.isBlank => (400, json(ApiError("The id of the table is blank")))
      case Right(request)                            =>
        arbiter.start(request.tableId) match
          case PromotionDecision.Granted(epoch) =>
            System.out.println(s"Table ${request.tableId}: its primary starts with epoch $epoch")
            (200, json(StartGranted(request.tableId, epoch)))
          case _                                =>
            (409, json(ApiError(s"Another node is playing table '${request.tableId}' now")))
