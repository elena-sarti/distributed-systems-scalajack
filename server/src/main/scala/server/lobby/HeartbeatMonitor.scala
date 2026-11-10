package server.lobby

import java.time.{Duration, Instant}

/** Marks as unavailable the tables that stopped sending heartbeats. Each call does one check, and how often to call it is
 * up to the caller.
 *
 * @param timeout how long a table may stay silent.
 * @param clock   the current time.
 */
final class HeartbeatMonitor(registry: TableRegistry, timeout: Duration, clock: () => Instant = Instant.now):

  /** @return the tables marked unavailable by this check. */
  def check(): Seq[TableInfo] =
    val silent = registry.markSilentUnavailable(clock().minus(timeout))
    silent.foreach(table => System.out.println(s"Table ${table.id} sent no heartbeat for ${timeout.toMillis} ms: marked unavailable"))
    silent
