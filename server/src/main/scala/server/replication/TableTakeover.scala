package server.replication

import server.{Cashier, TableHandler}
import scala.concurrent.duration.*

/** How a backup continues the table of a primary that has failed. */
object TableTakeover:

  /** How long the players of the failed primary have to come back to their seats. It starts when the table is taken over,
   * after the failure is detected, so it only covers the clients finding the new node. */
  val DefaultReturnWindow: FiniteDuration = 30.seconds

  /** The table as it was at the last state the backup stored, with every player seated and waiting for their client to come
   * back within `returnWindow` (see [[TableHandler.awaitReturn]]). Whoever does not come back is treated as a client that
   * drops.
   *
   * @param snapshot     the last state of the primary, or `None` if it never sent one.
   * @param cashier      it must not release the players at start-up, since they are still marked as seated.
   * @param returnWindow how long the players have to come back.
   */
  def resume(snapshot: Option[TableSnapshot], cashier: Cashier, returnWindow: FiniteDuration = DefaultReturnWindow): TableHandler =
    val table = TableHandler(cashier = cashier)
    snapshot.foreach: state =>
      table.restore(state)
      table.awaitReturn(returnWindow)
    table
