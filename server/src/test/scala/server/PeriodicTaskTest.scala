package server

import utils.TestExports.*
import java.time.Duration
import java.util.concurrent.atomic.AtomicInteger

class PeriodicTaskTest extends AnyFunSuite:

  private def waitUntil(condition: => Boolean, timeoutMillis: Long = 3000): Boolean =
    val deadline = System.currentTimeMillis() + timeoutMillis
    while !condition && System.currentTimeMillis() < deadline do Thread.sleep(5)
    condition

  private def withTask(period: Duration, runFirstNow: Boolean = false)(task: AtomicInteger => Unit)(check: (PeriodicTask, AtomicInteger) => Unit): Unit =
    val runs = AtomicInteger(0)
    val periodic = PeriodicTask("test-task", period, runFirstNow)(() => task(runs))
    try check(periodic, runs)
    finally periodic.stop()

  test("the task is repeated every period"):
    withTask(Duration.ofMillis(10))(_.incrementAndGet()): (periodic, runs) =>
      periodic.start()
      waitUntil(runs.get >= 3) shouldBe true

  test("the first run waits for one period, unless it is asked to run at once"):
    withTask(Duration.ofSeconds(30))(_.incrementAndGet()): (periodic, runs) =>
      periodic.start()
      Thread.sleep(100)
      runs.get shouldBe 0
    withTask(Duration.ofSeconds(30), runFirstNow = true)(_.incrementAndGet()): (periodic, runs) =>
      periodic.start()
      waitUntil(runs.get == 1) shouldBe true

  test("a task that fails is run again at the next period"):
    withTask(Duration.ofMillis(10))(runs => if runs.incrementAndGet() == 1 then throw RuntimeException("boom")): (periodic, runs) =>
      periodic.start()
      waitUntil(runs.get >= 3) shouldBe true

  test("after stop the task is not run any more"):
    withTask(Duration.ofMillis(10))(_.incrementAndGet()): (periodic, runs) =>
      periodic.start()
      waitUntil(runs.get >= 1) shouldBe true
      periodic.stop()
      Thread.sleep(50) // a run already started may still finish
      val atStop = runs.get
      Thread.sleep(100)
      runs.get shouldBe atStop
