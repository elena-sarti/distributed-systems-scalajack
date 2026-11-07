package server.replication

import server.{Cashier, TableHandler}
import scala.concurrent.duration.*

/** How a backup continues the table of a primary that has failed. */
object TableTakeover:

  /** How long the players of the failed primary have to come back to their seats. It starts when the table is taken over,
   * i.e. after the failure has already been detected, so it only has to cover the clients finding the new node. */
  val DefaultReturnWindow: FiniteDuration = 30.seconds

  /** The table as it was at the last state the backup stored, with every player still in their seat, waiting for
   * their client to come back (F7).
   *
   * The clients were connected to the failed primary, so none of them is here yet. Each one that comes back within
   * `returnWindow` gets its seat and goes on from the point where it was (see [[TableHandler.awaitReturn]]): the hand is
   * not touched while they are on their way. Whoever does not come back gets the treatment of a client that drops
   * (C5): from the deal on their hands stand and the seat is freed at the end of the hand; before the deal the
   * hand is cancelled for them and the bets go back to the players, who are not to blame for the failure. Either way
   * nobody stays marked as sitting at a table where nobody plays, and the table never blocks.
   *
   * @param snapshot the last state of the primary, or `None` if it never sent one: the table starts empty.
   * @param cashier  must not release the players at start-up, as a table that starts from scratch does: the ones
   *                 in the snapshot are still marked as sitting here in the database, and are freed as they leave.
   */
  def resume(snapshot: Option[TableSnapshot], cashier: Cashier, returnWindow: FiniteDuration = DefaultReturnWindow): TableHandler =
    val table = TableHandler(cashier = cashier)
    snapshot.foreach: state =>
      table.restore(state)
      table.awaitReturn(returnWindow)
    table
