package server

import java.time.Duration
import java.util.concurrent.{Executors, TimeUnit}
import scala.util.control.NonFatal

/** Runs a task again and again, on its own thread, with a pause of `period` between the end of a run and
 * the start of the next. Used by the table node (heartbeats) and by the lobby (silence checks), so the
 * scheduling is written once.
 *
 * A task that throws is logged and run again at the next period: a scheduled task that throws would
 * otherwise never run again, and the node would silently stop sending heartbeats.
 *
 * @param name        the name of the thread, to recognise it in a thread dump.
 * @param runFirstNow whether the first run is immediate instead of after one period.
 */
final class PeriodicTask(name: String, period: Duration, runFirstNow: Boolean = false)(task: () => Unit):
  // A daemon thread: it must not keep the JVM alive when the node is stopping.
  private val scheduler = Executors.newSingleThreadScheduledExecutor: runnable =>
    val thread = Thread(runnable, name)
    thread.setDaemon(true)
    thread

  def start(): Unit =
    val initialDelay = if runFirstNow then 0L else period.toMillis
    scheduler.scheduleWithFixedDelay(guarded, initialDelay, period.toMillis, TimeUnit.MILLISECONDS)

  /** No more runs; a run in progress is interrupted. */
  def stop(): Unit = scheduler.shutdownNow()

  private def guarded: Runnable = () =>
    try task()
    catch case NonFatal(error) => System.err.println(s"Error in periodic task $name: $error")
