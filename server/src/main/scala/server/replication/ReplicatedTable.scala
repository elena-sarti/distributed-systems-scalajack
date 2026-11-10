package server.replication

import protocol.ClientMessage
import protocol.ServerMessage
import protocol.ServerMessage.ErrorMessage
import server.{Cashier, Connection, SeatAvailability, SessionHandler, TableHandler}
import scala.collection.mutable.ListBuffer

/** A table whose every change reaches the backup before anybody outside can see it, which is passive replication with
 * output commit. The messages to the clients and the balances to save are held back until the backup stores the new
 * state. If it does not, the event is undone and the sender is told to try again.
 *
 * @param cashier    the cashier the table uses.
 * @param replicator how the new states are sent to the backup.
 * @param epoch      the epoch this primary starts with (see [[ReplicaUpdate]]).
 * @param newTable   builds the table on the cashier it must use.
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
   * From the next event on nothing waits for the backup. */
  def goOnAlone(newEpoch: Long): Unit =
    epochNow = newEpoch // before the flag: whoever sees the table alone also sees its new epoch
    alone = true

  /** The backup answers again (see [[BackupExcluder]]). Before the next event the table sends it its whole state. If the
   * backup stores it every move waits for it again, otherwise the table stays alone. */
  def takeBackupBack(): Unit = backupIsBack = true

  /** The lobby says that this primary has been replaced. From now on every event is refused, so nothing reaches the clients
   * or the database to contradict the new primary. */
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

  /** The players leave with their money only once the backup knows it, or if the table plays alone.
   * A replaced primary writes nothing. */
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

  /** The backup was left behind while the table played alone. The current state is all it needs since it replaces whatever
   * it had, and once stored it can take over again. */
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

    /** The connection as the table sees it, where what is sent waits. */
    def connection(client: Connection): Connection = new Connection:
      val id: String = client.id
      def send(message: ServerMessage): Unit = effects += (() => client.send(message))

    /** Sitting down happens at once since the table needs the balance, saving it can wait. */
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

    /** Undoes the event. Whoever sat down during it leaves with what they brought. */
    def discard(): Unit =
      val undone = seated.toList
      clear()
      undone.foreach((username, balance) => real.standUp(username, balance))

    private def clear(): Unit =
      effects.clear()
      seated.clear()
