package server

import utils.TestExports.*
import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}

class HealthServerTest extends AnyFunSuite:

  private def withServer(role: NodeRole)(check: Int => Unit): Unit =
    val server = HealthServer(role, 0) // port 0 = free port chosen by the OS
    server.start()
    try check(server.boundPort)
    finally server.stop()

  private def get(port: Int): HttpResponse[String] =
    HttpClient.newHttpClient().send(
      HttpRequest.newBuilder(URI.create(s"http://localhost:$port/health")).build(),
      HttpResponse.BodyHandlers.ofString()
    )

  NodeRole.values.foreach: role =>
    test(s"/health answers 200 and reports the role $role"):
      withServer(role): port =>
        val response = get(port)
        response.statusCode() shouldBe 200
        response.body() should include(role.toString.toLowerCase)
