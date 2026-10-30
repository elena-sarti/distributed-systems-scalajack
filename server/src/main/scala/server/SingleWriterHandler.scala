package server

import protocol.ClientMessage
import java.util.concurrent.{Executors, RejectedExecutionException, TimeUnit}
import scala.concurrent.duration.*
import scala.util.control.NonFatal

/** Makes a table a *single writer*: every event coming from the network (connection, message,
 * disconnection) is put in one queue and executed by one dedicated thread, in arrival order.
 *
 * The network threads only enqueue and return immediately, so a slow table never blocks them;
 * the wrapped handler is never called by two threads at the same time, so it can keep its state
 * in plain mutable fields with no locks (thread confinement).
 *
 * Time is one more kind of event: every `tickPeriod` the writer calls `table.onTick()` on the same thread,
 * so the table can notice that a decision is late without ever being touched by two threads at once.
 *
 * @param table      the table logic; it does not need to be thread-safe.
 * @param tickPeriod how often the table is told that time has passed.
 */
final class SingleWriterHandler(table: SessionHandler, tickPeriod: FiniteDuration = SingleWriterHandler.DefaultTickPeriod)
  extends SessionHandler:
  import SingleWriterHandler.*
  
  // A single-thread executor is exactly "one queue + one worker": tasks run one at a time, FIFO.
  // The scheduled variant keeps that guarantee: delayed and periodic tasks use the same single thread.
  private val writer = Executors.newSingleThreadScheduledExecutor(task => Thread(task, "table-writer"))
  writer.scheduleWithFixedDelay(guarded(table.onTick()), tickPeriod.toMillis, tickPeriod.toMillis, TimeUnit.MILLISECONDS)
  def onConnected(connection: Connection): Unit =
    submit(table.onConnected(connection))
  def onMessage(connection: Connection, message: ClientMessage): Unit =
    submit(table.onMessage(connection, message))
  def onDisconnected(connection: Connection): Unit =
    submit(table.onDisconnected(connection))

  /** Runs the events already queued, then stops the writer thread. Later events are ignored. */
  def stop(): Unit =
    writer.shutdown()
    writer.awaitTermination(StopTimeoutSeconds, TimeUnit.SECONDS)

  private def submit(event: => Unit): Unit =
    try writer.execute(guarded(event))
    catch case _: RejectedExecutionException => () // the table is shutting down: nothing left to do

  /** Wraps an event so that a bug in it is logged and does not stop the writer (a periodic task that
   * throws would otherwise never run again). */
  private def guarded(event: => Unit): Runnable = () =>
    try event
    catch case NonFatal(error) =>
      System.err.println(s"Error while processing a table event: $error")

object SingleWriterHandler:
  val DefaultTickPeriod: FiniteDuration = 1.second
  private val StopTimeoutSeconds = 5L