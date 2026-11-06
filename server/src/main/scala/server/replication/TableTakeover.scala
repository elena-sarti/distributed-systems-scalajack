package server.replication

import server.{Cashier, TableHandler}

/** How a backup continues the table of a primary that has failed. */
object TableTakeover:

  /** The table as it was at the last state the backup stored, with every player treated as disconnected.
   *
   * The clients were connected to the failed primary, so none of them is here: they get the same treatment as a client
   * that drops during a hand (C5). Their hands stand by themselves, the hand is settled without waiting for anybody,
   * and each seat is freed at the end of the hand (at once if no hand is being played), giving the player's balance
   * back through the cashier. So nobody stays marked as sitting at a table that nobody is playing at, and the table
   * never blocks. (Resuming the hand for a client that comes back is F7.)
   *
   * @param snapshot the last state of the primary, or `None` if it never sent one: the table starts empty.
   * @param cashier  must not release the players at start-up, as a table that starts from scratch does: the ones
   *                 in the snapshot are still marked as sitting here in the database, and are freed as they leave.
   */
  def resume(snapshot: Option[TableSnapshot], cashier: Cashier): TableHandler =
    val table = TableHandler(cashier = cashier)
    snapshot.foreach: state =>
      table.restore(state)
      table.connections.values.toList.foreach(table.onDisconnected)
    table
