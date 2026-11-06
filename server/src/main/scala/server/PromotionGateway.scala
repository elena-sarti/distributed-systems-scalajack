package server

/** What the lobby answers to a backup that asks to become the primary. */
enum PromotionAnswer:
  /** The lobby agrees: the backup is now the primary, with the epoch it asked for. */
  case Granted
  /** The lobby does not agree (HTTP status of the refusal): the backup stays a backup and can ask again. */
  case Denied(status: Int)
  /** No answer: without the lobby there is no majority, so nothing changes. */
  case Unreachable

/** How a backup node asks the lobby for the promotion. A trait of its own (and not part of [[LobbyGateway]]) because
 * only the backup uses it (ISP), and so that the promotion can be tested without a network. */
trait PromotionGateway:
  /** Never throws. */
  def requestPromotion(tableId: String, epoch: Long): PromotionAnswer
