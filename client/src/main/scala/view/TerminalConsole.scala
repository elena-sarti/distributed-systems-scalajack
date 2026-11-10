package view

import cats.Show
import cats.effect.{IO, Resource}
import cats.effect.std.{Console, Dispatcher, Queue}
import java.io.{BufferedReader, EOFException, InputStreamReader}
import java.nio.charset.Charset

/** The terminal of the online game. The keyboard is read by a thread of its own, and every line goes into a queue:
 * waiting for the player's answer can be cancelled, while a read from the keyboard cannot. */
final class TerminalConsole private (lines: Queue[IO, Option[String]]) extends Console[IO]:
  private val screen = Console.make[IO]

  override def readLine: IO[String] = lines.take.flatMap:
    case Some(line) => IO.pure(line)
    case None       => lines.offer(None) >> IO.raiseError(EOFException("The input is over"))

  def readLineWithCharset(charset: Charset): IO[String] = readLine

  /** Forgets what was typed before the question that is now on the screen. */
  def discardTypedAhead: IO[Unit] = lines.tryTakeN(None).flatMap(old => IO.whenA(old.contains(None))(lines.offer(None)))

  /** Flushed at once: an unfinished line, such as the time left rewritten in place, must show too. */
  def print[A](a: A)(using S: Show[A]): IO[Unit]   = IO.blocking { System.out.print(S.show(a)); System.out.flush() }
  def println[A](a: A)(using S: Show[A]): IO[Unit] = screen.println(a)
  def error[A](a: A)(using S: Show[A]): IO[Unit]   = screen.error(a)
  def errorln[A](a: A)(using S: Show[A]): IO[Unit] = screen.errorln(a)

object TerminalConsole:

  def resource: Resource[IO, TerminalConsole] =
    for
      dispatcher <- Dispatcher.sequential[IO]
      lines      <- Resource.eval(Queue.unbounded[IO, Option[String]])
      _          <- Resource.eval(IO(startReading(dispatcher, lines)))
    yield TerminalConsole(lines)

  /** A daemon thread: it never keeps the program alive, and it is blocked on the keyboard only. */
  private def startReading(dispatcher: Dispatcher[IO], lines: Queue[IO, Option[String]]): Unit =
    val reader = Thread(() =>
      val keyboard = BufferedReader(InputStreamReader(System.in))
      try
        Iterator.continually(keyboard.readLine()).takeWhile(_ != null).foreach(line => dispatcher.unsafeRunSync(lines.offer(Some(line))))
        dispatcher.unsafeRunSync(lines.offer(None))
      catch case _: Exception => () //the program is closing: nobody reads anymore
    , "keyboard")
    reader.setDaemon(true)
    reader.start()
