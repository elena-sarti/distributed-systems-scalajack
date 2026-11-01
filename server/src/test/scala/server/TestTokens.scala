package server

import server.auth.SessionTokens
import java.net.URI

/** The session tokens of the test tables, and a client that connects as a given user. */
object TestTokens:
  val tokens: SessionTokens = SessionTokens("the secret shared by the test lobby and tables".getBytes("UTF-8"))

  def connectAs(username: String, endpoint: TableEndpoint): TestWsClient =
    TestWsClient(URI.create(s"ws://localhost:${endpoint.boundPort}"), Some(tokens.issue(username)))
