import cats.effect._
import cats.effect.concurrent.Ref
import org.http4s._
import org.http4s.dsl.io._
import org.http4s.implicits._
import org.http4s.circe.CirceEntityCodec._
import io.circe.generic.auto._

import java.util.UUID
import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContext.global
import Game._
import cats.implicits.catsSyntaxApplicativeId

import scala.util.Random

object Game {
  final case class StartGame(min: Int, max: Int, countAttempt: Int)
  final case class Guess(id: String, number: Int, attempt: Int)
  final case class Result(guessed: Int)

  sealed trait ResultStep
  case object Greater extends ResultStep
  case object Lower extends ResultStep
  case object Win extends ResultStep
  case object GameOver extends ResultStep
}

object GuessServer extends IOApp {
  import org.http4s.server.blaze._
  import org.http4s.server.middleware.Logger

  def run(args: List[String]): IO[ExitCode] = for {
    app <- makeRoutes()
    server <- BlazeServerBuilder[IO](global)
      .withHttpApp(app)
      .bindHttp(port = 8080, host = "localhost")
      .serve
      .compile
      .drain
      .as(ExitCode.Success)
  } yield server

  def makeRoutes(): IO[HttpApp[IO]] = for {
    storage <- Ref.of[IO, Map[String, Int]](Map.empty)
    rts = routes(storage).orNotFound
    httpApp = Logger.httpApp(logHeaders = true, logBody = true)(rts)
  } yield httpApp


  def routes(storage: Ref[IO, Map[String, Int]]): HttpRoutes[IO] = HttpRoutes.of[IO] {

    case req @ POST -> Root / "start" =>
      Created(for {
        req <- req.as[StartGame]
        (id, riddle, attempt) = generateNumber(req)
        _ <- storage.update(_ + (id -> riddle))
        _ <- storage.get.map(x => x)
      } yield id
      )

    case req @ POST -> Root / "guess" => for {
      guess  <- req.as[Guess]
      result <- storage.modify { values =>
        val updated = values.updatedWith(guess.id)(x => x.filterNot(_ == guess.number))
        val a = values.get(guess.id).map(answer(_, guess.number, guess.attempt))
        (updated, a)
      }
      resp <- result.map(Ok(_)).getOrElse(NotFound("Not game"))
    } yield resp
  }

  def generateNumber(startGame: StartGame): (String, Int, Int) = {
    val riddle = Random.between(startGame.min,startGame.max)
    val attempt = 3
    (UUID.randomUUID.toString, riddle, attempt)
  }

  def answer(riddle: Int, guess: Int, attempts: Int): ResultStep = {
    if (guess == riddle && attempts >= 1) Win
    else if (attempts == 1) GameOver
    else if (guess > riddle && attempts != 1) Greater
    else Lower
  }
}

object HttpClient extends IOApp {
  import org.http4s.client.blaze.BlazeClientBuilder
  import org.http4s.client.Client
  import org.http4s.client.dsl.io._

  private val uri = uri"http://localhost:8080"

  private def printLine(string: String = ""): IO[Unit] = IO(println(string))

  private val min = 0
  private val max = 100
  private val attempt = 3

  def run(args: List[String]): IO[ExitCode] =
    BlazeClientBuilder[IO](ExecutionContext.global).resource
      .use { case client =>
      for {
        _  <- printLine("Start")
        id <- client.expect[String](Method.POST(StartGame(min, max, attempt), uri / "start"))
        _  <- guess(id, min, max, attempt)(client)
        _  <- printLine()
      } yield ()
    }.as(ExitCode.Success)


  def guess(id: String, min: Int, max: Int, attempt: Int)(client: Client[IO]): IO[Result] = {
    val x = (min + max) / 2
    val attempts = attempt - 1
    println(attempt)
    client.expect[ResultStep](Method.POST(Guess(id, x, attempts), uri / "guess")).flatMap {
      case Win => println(s"WIN!!! number = $x"); Result(x).pure[IO]
      case GameOver => println(s"Game Over"); Result(x).pure[IO]
      case Greater => guess(id, min, x, attempts)(client)
      case Lower => guess(id, x, max, attempts)(client)
    }
  }
}

