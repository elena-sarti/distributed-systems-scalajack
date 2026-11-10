package server

import protocol.ClientMessage
import java.util.concurrent.{Executors, RejectedExecutionException, TimeUnit}
import scala.concurrent.duration.*
import scala.util.control.NonFatal

/** Makes a table a single writer. Connections, messages, disconnections and time ticks are queued and executed in
 * arrival order by one thread, so the wrapped handler needs no locks and the network threads never block.
 *
 * @param table      the table logic, which does not need to be thread-safe.
 * @param tickPeriod how often the table is told that time has passed.
 */
final class SingleWriterHandler(table: SessionHandler, tickPeriod: FiniteDuration = SingleWriterHandler.DefaultTickPeriod)
  extends SessionHandler:
  import SingleWriterHandler.*
  
  // A single-thread executor is one queue + one worker: tasks run one at a time, FIFO.
  // The scheduled variant keeps that guarantee: delayed and periodic tasks use the same single thread.
  private val writer = Executors.newSingleThreadScheduledExecutor(task => Thread(task, "table-writer"))
  writer.scheduleWithFixedDelay(guarded(table.onTick()), tickPeriod.toMillis, tickPeriod.toMillis, TimeUnit.MILLISECONDS)
  def onConnected(connection: Connection): Unit =
    submit(table.onConnected(connection))
  def onMessage(connection: Connection, message: ClientMessage): Unit =
    submit(table.onMessage(connection, message))
  def onDisconnected(connection: Connection): Unit =
    submit(table.onDisconnected(connection))

  /** Runs the events already queued, then lets the table stop and stops the writer thread.
   * Later events are ignored. */
  def stop(): Unit =
    submit(table.onStop())
    writer.shutdown()
    writer.awaitTermination(StopTimeoutSeconds, TimeUnit.SECONDS)

  private def submit(event: => Unit): Unit =
    try writer.execute(guarded(event))
    catch case _: RejectedExecutionException => () // the table is shutting down: nothing left to do

  /** Wraps an event so that an error in it is logged and does not stop the writer. */
  private def guarded(event: => Unit): Runnable = () =>
    try event
    catch case NonFatal(error) =>
      System.err.println(s"Error while processing a table event: $error")

object SingleWriterHandler:
  val DefaultTickPeriod: FiniteDuration = 1.second
  private val StopTimeoutSeconds = 5L