import cats.effect.{ExitCode, IO, IOApp, Resource}

import scala.io.Source

trait Main[T] extends IOApp {

  val File: String

  val `2020`: Int = 2020

  override def run(args: List[String]): IO[ExitCode] = {
    for {
      lines <- Resource.fromAutoCloseable(IO(Source.fromFile(File)))
        .use(f => IO(f.getLines.toList))
      result = solve(lines)
      _ <- IO(result.foreach(println(_)))
    } yield ExitCode.Success
  }

  def solve(input: List[String]): List[T]

}
