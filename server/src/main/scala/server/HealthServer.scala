package server

import com.sun.net.httpserver.HttpServer
import java.net.InetSocketAddress

/** Minimal liveness endpoint, based on the JDK's built-in HTTP server (no extra dependencies). */
final class HealthServer(role: NodeRole, port: Int):
  private val http = HttpServer.create(InetSocketAddress(port), 0)

  http.createContext("/health", exchange =>
    val body = s"""{"status":"UP","role":"${role.toString.toLowerCase}"}""".getBytes("UTF-8")
    exchange.getResponseHeaders.add("Content-Type", "application/json")
    exchange.sendResponseHeaders(200, body.length)
    exchange.getResponseBody.write(body)
    exchange.close()
  )

  def start(): Unit    = http.start()
  def stop(): Unit     = http.stop(0)
  def boundPort: Int   = http.getAddress.getPort