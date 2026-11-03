package server.lobby

import java.time.{Duration, Instant}

/** Finds the tables that stopped sending heartbeats and marks them unavailable, so that nobody is sent
 * there. A table that sends a heartbeat again becomes available (that is done by the registry).
 *
 * It does one check at a time: how often it is called is up to the caller (see `PeriodicTask`).
 *
 * @param timeout how long a table may stay silent.
 * @param clock   the current time; the tests replace it.
 */
final class HeartbeatMonitor(registry: TableRegistry, timeout: Duration, clock: () => Instant = Instant.now):

  /** @return the tables marked unavailable by this check. */
  def check(): Seq[TableInfo] =
    val silent = registry.markSilentUnavailable(clock().minus(timeout))
    silent.foreach(table => System.out.println(s"Table ${table.id} sent no heartbeat for ${timeout.toMillis} ms: marked unavailable"))
    silent
