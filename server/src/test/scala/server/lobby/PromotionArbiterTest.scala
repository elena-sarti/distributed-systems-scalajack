package server.lobby

import utils.TestExports.*
import scala.collection.mutable.ListBuffer

class PromotionArbiterTest extends AnyFunSuite with BeforeAndAfterEach:

  private var registry: TableRegistry   = _
  private var granted: ListBuffer[String] = _
  private var arbiter: PromotionArbiter = _

  override def beforeEach(): Unit =
    registry = TableRegistry()
    granted = ListBuffer.empty
    arbiter = PromotionArbiter(registry, id => granted += id)
    registry.register("table-1", "host", 9091)

  private def primaryIsSilent(): Unit = registry.markStatus("table-1", TableStatus.Unavailable)

  test("the promotion is granted when the lobby does not hear the primary either"):
    primaryIsSilent()
    arbiter.decide("table-1", 5) shouldBe PromotionDecision.Granted(5)
    granted.toList shouldBe List("table-1")

  test("the promotion is refused while the lobby still hears the primary: the backup is just cut off from it"):
    arbiter.decide("table-1", 5) shouldBe PromotionDecision.PrimaryAlive
    granted shouldBe empty

  test("a table the lobby does not know can not be promoted"):
    arbiter.decide("ghost", 5) shouldBe PromotionDecision.UnknownTable
    granted shouldBe empty

  test("the same promotion is not granted twice, and an older epoch never undoes a newer one"):
    primaryIsSilent()
    arbiter.decide("table-1", 5) shouldBe PromotionDecision.Granted(5)
    arbiter.decide("table-1", 5) shouldBe PromotionDecision.StaleEpoch(5)
    arbiter.decide("table-1", 4) shouldBe PromotionDecision.StaleEpoch(5)
    granted should have size 1

  test("a later failure of the new primary can be promoted with a greater epoch"):
    primaryIsSilent()
    arbiter.decide("table-1", 5)
    arbiter.decide("table-1", 6) shouldBe PromotionDecision.Granted(6)

  test("the epochs are kept for each table"):
    registry.register("table-2", "host", 9092)
    primaryIsSilent()
    registry.markStatus("table-2", TableStatus.Unavailable)
    arbiter.decide("table-1", 5)
    arbiter.decide("table-2", 5) shouldBe PromotionDecision.Granted(5)

  test("two backups asking at the same moment: exactly one is granted"):
    primaryIsSilent()
    val results = java.util.concurrent.ConcurrentLinkedQueue[PromotionDecision]()
    val task: Runnable = () =>
      results.add(arbiter.decide("table-1", 5))
      ()
    val threads = (1 to 8).map(_ => Thread(task))
    threads.foreach(_.start())
    threads.foreach(_.join())
    results.toArray.count(_.isInstanceOf[PromotionDecision.Granted]) shouldBe 1

  test("a primary that cannot reach its backup is let go on alone with a newer epoch, while the lobby hears it"):
    arbiter.exclude("table-1", 5) shouldBe PromotionDecision.Granted(5)
    granted shouldBe empty // nobody has been promoted

  test("a primary that has been replaced is not let go on alone"):
    primaryIsSilent()
    arbiter.decide("table-1", 5) // the backup has taken its place with the epoch that follows the primary's
    arbiter.exclude("table-1", 5) shouldBe PromotionDecision.StaleEpoch(5)

  test("once the backup is out it can no longer be promoted, not even if the primary then goes silent"):
    arbiter.exclude("table-1", 5)
    primaryIsSilent()
    arbiter.decide("table-1", 5) shouldBe PromotionDecision.StaleEpoch(5) // it still has the epoch before the exclusion
    granted shouldBe empty

  test("the backup of a table the lobby does not know cannot be excluded"):
    arbiter.exclude("ghost", 5) shouldBe PromotionDecision.UnknownTable

  test("backup and primary cut off from each other ask at the same moment: exactly one of them wins"):
    primaryIsSilent() // the worst case: the lobby would also grant the promotion
    val results = java.util.concurrent.ConcurrentLinkedQueue[PromotionDecision]()
    val promote: Runnable = () => { results.add(arbiter.decide("table-1", 5)); () }
    val exclude: Runnable = () => { results.add(arbiter.exclude("table-1", 5)); () }
    val threads = (1 to 4).flatMap(_ => List(Thread(promote), Thread(exclude)))
    threads.foreach(_.start())
    threads.foreach(_.join())
    results.toArray.count(_.isInstanceOf[PromotionDecision.Granted]) shouldBe 1
