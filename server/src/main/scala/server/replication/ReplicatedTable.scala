package server.replication

import protocol.ClientMessage
import protocol.ServerMessage
import protocol.ServerMessage.ErrorMessage
import server.{Cashier, Connection, SeatAvailability, SessionHandler, TableHandler}
import scala.collection.mutable.ListBuffer

/** A table whose every change reaches the backup before anybody outside can see it: passive replication
 * with output commit.
 *
 * Each event runs on the table as usual, but what the table would send to the clients and the balances it
 * would save are held back. If the state has changed it is sent to the backup, and only when the backup has
 * stored it are those effects let out. If the backup does not store it the event is undone, as if it had never
 * arrived, and whoever sent it is told to try again. So the clients and the database never see a state the backup
 * does not have, and the backup can go on from wherever the primary stops.
 *
 * Like the table, it runs on the single writer: the wait for the backup holds up the whole table, on purpose.
 *
 * @param epoch    the epoch this primary starts with (see [[ReplicaUpdate]]).
 * @param newTable builds the table on the cashier it must use.
 */
final class ReplicatedTable(cashier: Cashier, replicator: Replicator, epoch: Long,
                            newTable: Cashier => TableHandler = cashier => TableHandler(cashier = cashier))
  extends SessionHandler, SeatAvailability:
  import ReplicatedTable.*

  private val held     = HeldEffects(cashier)
  private val table    = newTable(held.cashier)
  private var sequence = 0L

  // Changed by the liveness thread when the lobby lets the table go on without its backup, read by the writer.
  @volatile private var epochNow = epoch
  @volatile private var alone    = false
  // Set by the liveness thread when the backup answers again; the writer brings it up to date at the next event.
  @volatile private var backupIsBack = false
  // Set when the lobby says that another node plays this table now.
  @volatile private var replaced = false
  /** Whether the backup has ever stored a state of this primary (only the writer uses it). */
  private var backupHasState = false

  def freeSeats: Int = table.freeSeats

  /** The epoch of this primary now: it is sent with every update and every heartbeat. */
  def currentEpoch: Long = epochNow

  /** Whether the table is playing without a backup (degraded mode). */
  def isAlone: Boolean = alone

  /** The lobby has agreed that the table goes on without its backup, with `newEpoch` (see [[BackupExcluder]]).
   * From the next event on nothing waits for the backup: the effects go out as soon as the table produces them. */
  def goOnAlone(newEpoch: Long): Unit =
    epochNow = newEpoch // before the flag: whoever sees the table alone also sees its new epoch
    alone = true

  /** The backup answers again (see [[BackupExcluder]]). At the next event, before it, the table sends it its whole
   * state: if the backup stores it, from then on every move waits for the backup again. If not, the table stays alone
   * and can be asked again. */
  def takeBackupBack(): Unit = backupIsBack = true

  /** The lobby says that this primary has been replaced: its epoch is old, another node plays the table now (e.g.
   * this one was cut off and its backup has been promoted). From now on nothing this table does may reach the
   * clients or the database, where it would contradict the new primary: every event is refused. */
  def stepDown(): Unit = replaced = true

  def isReplaced: Boolean = replaced

  def onConnected(connection: Connection): Unit =
    replicated(None)(table.onConnected(held.connection(connection)))
  def onMessage(connection: Connection, message: ClientMessage): Unit =
    replicated(Some(connection))(table.onMessage(held.connection(connection), message))
  def onDisconnected(connection: Connection): Unit =
    replicated(None)(table.onDisconnected(held.connection(connection)))
  override def onTick(): Unit =
    // the backup learns the epoch of this primary at once, even before the first move: without it, it could not be
    // promoted if the primary failed before anybody played
    if !replaced && !alone && !backupHasState then replicate()
    replicated(None)(table.onTick())

  /** The node is going down: the players leave with their money, as with a move, only once the backup knows it (or
   * if the table is playing alone). If the backup cannot be told, nothing is written: the backup still has the
   * players, and if it takes over it settles them itself. A primary that has been replaced writes nothing at all. */
  override def onStop(): Unit =
    if !replaced then
      table.onStop()
      if alone || replicate() then held.release() else held.discard()

  private def replicated(sender: Option[Connection])(event: => Unit): Unit =
    if replaced then sender.foreach(_.send(ErrorMessage(TableMoved)))
    else play(sender)(event)

  private def play(sender: Option[Connection])(event: => Unit): Unit =
    if alone && backupIsBack then resynchronise()
    val before      = table.snapshot
    val connections = table.connections
    event
    if alone || table.snapshot == before || replicate() then held.release()
    else
      held.discard()
      table.restore(before, connections.get)
      sender.foreach(_.send(ErrorMessage(TablePaused)))

  /** The backup was left behind while the table played alone: the current state is all it needs, since it replaces
   * whatever the backup had. Once stored, the backup has every move again, so it can again take over if needed. */
  private def resynchronise(): Unit =
    backupIsBack = false
    if replicate() then
      alone = false
      System.out.println(s"The backup has the whole table again (epoch $epochNow): every move waits for it again")

  private def replicate(): Boolean =
    sequence += 1
    val stored = replicator.replicate(ReplicaUpdate(epochNow, sequence, table.snapshot))
    if stored then backupHasState = true
    stored

object ReplicatedTable:
  val TablePaused = "The table cannot save your move right now, try again in a moment"
  val TableMoved  = "This table is now played on another node: ask the lobby where it is"

  /** The effects of an event that must wait for the backup, in the order the table produced them. */
  private final class HeldEffects(real: Cashier):
    private val effects = ListBuffer[() => Unit]()
    private val seated  = ListBuffer[(String, Double)]()

    /** The connection as the table sees it: what is sent to it waits. */
    def connection(client: Connection): Connection = new Connection:
      val id: String = client.id
      def send(message: ServerMessage): Unit = effects += (() => client.send(message))

    /** Sitting down has to happen at once, because the table needs the balance; saving it can wait. */
    val cashier: Cashier = new Cashier:
      def sitDown(username: String): Either[String, Double] =
        val result = real.sitDown(username)
        result.foreach(balance => seated += username -> balance)
        result
      def standUp(username: String, balance: Double): Unit =
        effects += (() => real.standUp(username, balance))

    def release(): Unit =
      val now = effects.toList
      clear()
      now.foreach(_())

    /** The event is undone: whoever sat down during it is not at the table, so they leave with what they brought. */
    def discard(): Unit =
      val undone = seated.toList
      clear()
      undone.foreach((username, balance) => real.standUp(username, balance))

    private def clear(): Unit =
      effects.clear()
      seated.clear()
