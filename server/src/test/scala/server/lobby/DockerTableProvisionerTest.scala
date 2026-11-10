package server.lobby

import utils.TestExports.*
import java.io.IOException
import java.nio.file.Path
import java.time.Duration

class DockerTableProvisionerTest extends AnyFunSuite with BeforeAndAfterEach:

  private var daemon: FakeDockerDaemon = _

  override def beforeEach(): Unit =
    daemon =
      try FakeDockerDaemon()
      catch case _: UnsupportedOperationException | _: IOException => cancel("Unix domain sockets are not available here")

  override def afterEach(): Unit = if daemon != null then daemon.stop()

  private def provisioner(containerName: String => String = identity, timeout: Duration = Duration.ofSeconds(5)) =
    DockerTableProvisioner(daemon.socket, timeout, containerName)

  test("starting a table asks the daemon to start the container of that name"):
    provisioner().start("table-2") shouldBe true
    daemon.requests.toArray.toList shouldBe List("POST /containers/table-2/start HTTP/1.1")

  test("the container name can differ from the id of the table"):
    provisioner(id => s"scalajack-$id-1").start("table-2") shouldBe true
    daemon.requests.toArray.toList shouldBe List("POST /containers/scalajack-table-2-1/start HTTP/1.1")

  test("a container that was already running is a success too"):
    daemon.status = 304
    provisioner().start("table-2") shouldBe true

  test("a container that does not exist, or a daemon that fails, is a failure"):
    Seq(404, 500).foreach: status =>
      daemon.status = status
      provisioner().start("table-2") shouldBe false

  test("without a daemon on the socket the start fails, it does not throw"):
    DockerTableProvisioner(Path.of("no-such-directory", "docker.sock")).start("table-2") shouldBe false

  test("a daemon that does not answer makes the start fail at the timeout, without blocking the caller for long"):
    daemon.answers = false
    val begin = System.nanoTime()
    provisioner(timeout = Duration.ofMillis(300)).start("table-2") shouldBe false
    Duration.ofNanos(System.nanoTime() - begin).toMillis should be < 3000L

  test("a name that could change the path of the request is refused without calling the daemon"):
    Seq("", "a/b", "../x", "a b", "-a").foreach: id =>
      provisioner().start(id) shouldBe false
    daemon.requests shouldBe empty

  private def withBackups = DockerTableProvisioner(daemon.socket, backupName = id => Some(s"$id-backup"))

  test("a table that has a backup is started after its backup, so that it finds it at once"):
    withBackups.start("table-2") shouldBe true
    daemon.requests.toArray.toList shouldBe
      List("POST /containers/table-2-backup/start HTTP/1.1", "POST /containers/table-2/start HTTP/1.1")

  test("a table whose backup does not exist, or cannot be started, starts anyway"):
    daemon.missing = Set("table-2-backup")
    withBackups.start("table-2") shouldBe true
    daemon.requests should have size 2

  test("if the table itself cannot be started, the start fails even when its backup has started"):
    daemon.missing = Set("table-2")
    withBackups.start("table-2") shouldBe false

