package server

import com.sun.net.httpserver.{HttpHandler, HttpServer}
import java.net.InetSocketAddress
import java.util.concurrent.Executors

/** The HTTP server of a node, based on the one built into the JDK. Every node answers `/health` and may add the routes
 * of its role.
 *
 * @param routes the extra paths served by this node, with their handlers.
 */
final class HealthServer(role: NodeRole, port: Int, routes: Map[String, HttpHandler] = Map.empty):
  private val http = HttpServer.create(InetSocketAddress(port), 0)

  // By default the JDK server handles one request at a time on its own thread: a slow request
  // (hashing a password takes 0.2 s) would delay all the others, /health included. A small pool
  // serves them in parallel; the handlers are thread-safe.
  private val workers = Executors.newFixedThreadPool(HealthServer.WorkerThreads)
  http.setExecutor(workers)

  http.createContext("/health", exchange =>
    val body = s"""{"status":"UP","role":"${role.toString.toLowerCase}"}""".getBytes("UTF-8")
    exchange.getResponseHeaders.add("Content-Type", "application/json")
    exchange.sendResponseHeaders(200, body.length)
    exchange.getResponseBody.write(body)
    exchange.close()
  )
  routes.foreach((path, handler) => http.createContext(path, handler))

  def start(): Unit = http.start()
  def stop(): Unit =
    http.stop(0)
    workers.shutdown()
  def boundPort: Int = http.getAddress.getPort

object HealthServer:
  private val WorkerThreads = 4
