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
