import cats.effect._
import cats.implicits.{catsSyntaxApplicativeId, catsSyntaxFlatMapOps}
import io.circe.{Decoder, Encoder, jawn}
import fs2.Pipe
import fs2.concurrent.Queue
import org.http4s.{HttpApp, HttpRoutes}
import org.http4s.dsl.io.{->, /, GET, Root}
import org.http4s.implicits.http4sLiteralsSyntax
import org.http4s.websocket.WebSocketFrame
import org.http4s.implicits.http4sKleisliResponseSyntaxOptionT
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.server.websocket.WebSocketBuilder
import org.http4s.client.jdkhttpclient.{JdkWSClient, WSConnectionHighLevel, WSFrame, WSRequest}

import scala.concurrent.ExecutionContext
import scala.util.Random
import java.net.http.HttpClient
import java.util.UUID
import Game._
import cats.effect.concurrent.Ref
import fs2.io.Watcher.Event.Created
import io.circe
import io.circe.parser.decode
import io.circe.syntax.EncoderOps
import org.http4s.server.middleware.Logger

import scala.concurrent.ExecutionContext.global

object Game {
  final case class StartGame(min: Int, max: Int, countAttempt: Int)
  object StartGame {
    val decodeStartGame: Decoder[StartGame] =
      Decoder.forProduct3("min", "max", "countAttempt")(StartGame.apply)

    val encodeStareGame: Encoder[StartGame] =
      Encoder.forProduct3("min", "max", "countAttempt")(u =>
        (u.min, u.max, u.countAttempt))
  }
  final case class Guess(id: String, number: Int = 0, attempt: Int = 0, riddle: Int = 0)
  object Guess{
    val decodeGuess: Decoder[Guess] =
      Decoder.forProduct4("id", "number", "attempt", "riddle")(Guess.apply)

    val encodeGuess: Encoder[Guess] =
      Encoder.forProduct4("id", "number", "attempt", "riddle")(u => (u.id, u.number, u.attempt, u.riddle))
  }
  final case class Result(guessed: Int)

  sealed trait ResultStep
  case object Greater extends ResultStep
  case object Lower extends ResultStep
  case object Win extends ResultStep
  case object GameOver extends ResultStep
}

object WebSocketServer extends IOApp {

  override def run(args: List[String]): IO[ExitCode] = for {
    app <- makeRoutes()
    server <- BlazeServerBuilder[IO](global)
      .withHttpApp(app)
      .bindHttp(port = 9002, host = "localhost")
      .serve
      .compile
      .drain
      .as(ExitCode.Success)
  } yield server

  def makeRoutes(): IO[HttpApp[IO]] = for {
    storage <- Ref.of[IO, Map[String, Int]](Map.empty)
    rts = routs(storage).orNotFound
    httpApp = Logger.httpApp(logHeaders = true, logBody = true)(rts)
  } yield httpApp

  //  private def httpApp = echoRoute.orNotFound

  private def routs (storage: Ref[IO, Map[String, Int]]): HttpRoutes[IO] = HttpRoutes.of[IO] {
    case GET -> Root / "echo" =>
      val echoPipe: Pipe[IO, WebSocketFrame, WebSocketFrame] =
        _.collect {
          case WebSocketFrame.Text(message, _) =>
            val game = decode(message)(StartGame.decodeStartGame)
            game match {
              case Right(value) =>
                val (id, riddle) = generateNumber(value)
                for{
                  _ <- storage.update(_ + (id -> riddle))
                  _ <- storage.get.map(x => x)
                }yield ()
                WebSocketFrame.Text(Guess(id = id,riddle = riddle).asJson(Guess.encodeGuess).toString)
              case Left(value) => decode(message)(Guess.decodeGuess) match {
                case Right(value) => WebSocketFrame.Text(answer(value).toString)
                case Left(value) => WebSocketFrame.Text("Error")
              }
            }
        }

      for {
        queue <- Queue.unbounded[IO, WebSocketFrame]
        response <- WebSocketBuilder[IO].build(
          receive = queue.enqueue,
          send = queue.dequeue.through(echoPipe)
        )
      } yield response
  }

  private def generateNumber(startGame: StartGame): (String, Int) = {
    val riddle = Random.between(startGame.min, startGame.max)
    (UUID.randomUUID.toString, riddle)
  }

  private def answer(guess: Guess): ResultStep = {
        if (guess.riddle == guess.number && guess.attempt >= 0) Win
        else if (guess.attempt == 0) GameOver
        else if (guess.riddle < guess.number && guess.attempt != 0) Greater
        else Lower
  }
}


object WebSocketClient extends IOApp {

  private val uri = uri"ws://localhost:9002/echo"

  private def printLine(string: String = ""): IO[Unit] = IO(println(string))

  private val min = 0
  private val max = 100
  private val attempt = 3

  override def run(args: List[String]): IO[ExitCode] = {
    val clientResource = Resource.eval(IO(HttpClient.newHttpClient()))
      .flatMap(JdkWSClient[IO](_).connectHighLevel(WSRequest(uri)))

    clientResource.use { client =>
      for {
        _ <- client.send(WSFrame.Text(StartGame(min,max,attempt).asJson(StartGame.encodeStareGame).toString()))
        message <- client.receiveStream.collectFirst { case WSFrame.Text(s, _) => s }.compile.string
        guees = decode(message)(Guess.decodeGuess).toOption
        _ <- guess(guees.map(_.id).get, min, max, attempt, guees.map(_.riddle).get)(client)
      } yield ExitCode.Success
    }
  }

  def guess(id: String, min: Int, max: Int, attempt: Int, riddle: Int)(client: WSConnectionHighLevel[IO]): IO[Result] = {
    val x = (min + max) / 2
    val attempts = attempt - 1
    for{
      _ <- client.send(WSFrame.Text(Guess(id, x,attempts, riddle).asJson(Guess.encodeGuess).toString()))
      rsIo <- client.receive
      rs = rsIo match {
        case Some(WSFrame.Text(message, _)) => message.trim
      }
      _ <- printLine(s"id: $id, attempt:$attempts number:$x: result:$rs")
      res <- rs match {
        case "Win" => println(s"WIN!!! number = $x"); Result(x).pure[IO]
        case "GameOver" => println(s"Game Over"); Result(x).pure[IO]
        case "Greater" => guess(id, min, x, attempts, riddle)(client)
        case "Lower" => guess(id, x, max, attempts, riddle)(client)
      }
    } yield res
  }

}