package demo

import java.time.{Duration, Instant}
import java.util.concurrent.TimeUnit
import scala.io.Source

/** The few Docker commands the demo needs, run with the docker client of this machine: the deployment is the one of
 * docker-compose.yml, started from the root of the repository. */
object Docker:

  /** Exit code and output (standard output and error together) of docker args.... */
  def run(args: String*): (Int, String) =
    val process = ProcessBuilder(("docker" +: args)*).redirectErrorStream(true).start()
    val output  = Source.fromInputStream(process.getInputStream, "UTF-8").mkString
    if !process.waitFor(3, TimeUnit.MINUTES) then process.destroyForcibly()
    (process.exitValue(), output)

  /** Runs the command and fails with its output if Docker refuses it. */
  def must(args: String*): String =
    val (code, output) = run(args*)
    if code != 0 then throw IllegalStateException(s"docker ${args.mkString(" ")} failed:\n$output")
    output

  /** Recreates the whole deployment, as at its first start: lobby, database, table-1 and its backup. */
  def freshDeployment(): Unit = must("compose", "up", "-d", "--force-recreate", "--wait")

  /** A crash: the process is killed at once, without the chance to do anything before dying. */
  def kill(container: String): Unit = must("kill", container)

  /** The network of the deployment, the one the container is attached to. */
  def networkOf(container: String): String =
    must("inspect", "-f", "{{range $name, $net := .NetworkSettings.Networks}}{{$name}} {{end}}", container).trim.split(" ").head

  /** A network partition: the container is still running, but nobody can reach it and it can reach nobody. */
  def isolate(container: String, network: String): Unit = must("network", "disconnect", network, container)

  /** The partition heals. */
  def reconnect(container: String, network: String): Unit = must("network", "connect", network, container)

  /** The log of a service of the deployment. */
  def logLines(service: String): List[String] = run("compose", "logs", "--no-log-prefix", service)._2.linesIterator.toList

  /** How many lines of the log satisfy `wanted`: taken before an action, it tells which lines come after it. */
  def countLog(service: String)(wanted: String => Boolean): Int = logLines(service).count(wanted)

  /** Waits until more than `already` lines of the log satisfy `wanted`.
   * @return how long it took, or `None` if nothing came within the timeout. */
  def awaitLog(service: String, timeout: Duration, already: Int = 0)(wanted: String => Boolean): Option[Duration] =
    val start = Instant.now()
    val end   = start.plus(timeout)
    var found = false
    while !found && Instant.now().isBefore(end) do
      found = countLog(service)(wanted) > already
      if !found then Thread.sleep(500)
    Option.when(found)(Duration.between(start, Instant.now()))
