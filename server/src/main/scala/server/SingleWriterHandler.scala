package server

import protocol.ClientMessage
import java.util.concurrent.{Executors, RejectedExecutionException, TimeUnit}
import scala.util.control.NonFatal

/** Makes a table a *single writer*: every event coming from the network (connection, message,
 * disconnection) is put in one queue and executed by one dedicated thread, in arrival order.
 *
 * The network threads only enqueue and return immediately, so a slow table never blocks them;
 * the wrapped handler is never called by two threads at the same time, so it can keep its state
 * in plain mutable fields with no locks (thread confinement).
 *
 * @param table the table logic; it does not need to be thread-safe.
 */
final class SingleWriterHandler(table: SessionHandler) extends SessionHandler:
  import SingleWriterHandler.*

  // A single-thread executor is exactly "one queue + one worker": tasks run one at a time, FIFO.
  private val writer = Executors.newSingleThreadExecutor(task => Thread(task, "table-writer"))

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
    val task: Runnable = () =>
      try event
      catch case NonFatal(error) => // a bug in one event must not stop the table: log it and go on
        System.err.println(s"Error while processing a table event: $error")
    try writer.execute(task)
    catch case _: RejectedExecutionException => () // the table is shutting down: nothing left to do

object SingleWriterHandler:
  private val StopTimeoutSeconds = 5L
