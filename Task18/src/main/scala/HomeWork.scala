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
  final case class Guess(number: Int, attempt: Int)
  object Guess{
    val decodeGuess: Decoder[Guess] =
      Decoder.forProduct2("number", "attempt")(Guess.apply)

    val encodeGuess: Encoder[Guess] =
      Encoder.forProduct2("number", "attempt")(u => (u.number, u.attempt))
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
                case Right(value) => val riddle = generateNumber(value); WebSocketFrame.Text(riddle.toString())
                case Left(value) => decode(message)(Guess.decodeGuess) match {
                  case Right(value) => WebSocketFrame.Text(answer(riddle = 35, value.number, value.attempt).toString())
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

  private def generateNumber(startGame: StartGame): Int = {
    val riddle = Random.between(startGame.min,startGame.max)
    riddle
  }

  private def answer(riddle: Int, guess: Int, attempts: Int): ResultStep = {
    if (guess == riddle && attempts >= 0) Win
    else if (attempts == 0) GameOver
    else if (guess > riddle && attempts != 0) Greater
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
        _ <- client.receiveStream.collectFirst { case WSFrame.Text(s, _) => s }.compile.string >>= printLine
        _ <- guess(min, max, attempt)(client)
      } yield ExitCode.Success
    }
  }

  def guess(min: Int, max: Int, attempt: Int)(client: WSConnectionHighLevel[IO]): IO[Result] = {
    val x = (min + max) / 2
    val attempts = attempt - 1
   for{
     _ <- client.send(WSFrame.Text(Guess(x,attempts).asJson(Guess.encodeGuess).toString()))
     rsIo <- client.receive
     rs = rsIo match {
       case Some(WSFrame.Text(message, _)) => message.trim
     }
     _ <- printLine(s"attempt:$attempts number:$x: result:$rs")
     res <- rs match {
         case "Win" => println(s"WIN!!! number = $x"); Result(x).pure[IO]
         case "GameOver" => println(s"Game Over"); Result(x).pure[IO]
         case "Greater" => guess(min, x, attempts)(client)
         case "Lower" => guess(x, max, attempts)(client)
     }
   } yield res
  }

}