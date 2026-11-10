package server

/** What the lobby answers to a backup that asks to become the primary. */
enum PromotionAnswer:
  /** The lobby agrees, so the backup is now the primary with the epoch it asked for. */
  case Granted
  /** The lobby does not agree, with the HTTP status of the refusal. The backup stays a backup and can ask again. */
  case Denied(status: Int)
  /** No answer: without the lobby there is no majority, so nothing changes. */
  case Unreachable

/** How a backup node asks the lobby for the promotion. It is separate from [[LobbyGateway]] because only the backup uses
 * it. */
trait PromotionGateway:
  def requestPromotion(tableId: String, epoch: Long): PromotionAnswer

/** What the lobby answers to a replicated table node that starts. */
enum StartAnswer:
  /** The node is the primary of the table, with this epoch. */
  case Granted(epoch: Long)
  /** Another node plays the table now, with the HTTP status of the refusal: the node must wait. */
  case Denied(status: Int)
  case Unreachable

/** How a replicated table node gets its epoch from the lobby when it starts. */
trait StartGateway:
  def requestStart(tableId: String): StartAnswer

/** How a primary node asks the lobby to go on without its backup. The answers mean the same as for a promotion. */
trait ExclusionGateway:
  def requestExclusion(tableId: String, epoch: Long): PromotionAnswer
