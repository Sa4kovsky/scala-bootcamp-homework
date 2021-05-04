import DbCommon.{createTableAuthorsSql, createTableBooksSql, populateDataSql}
import cats.effect.{Bracket, Effect, ExitCode, IO, IOApp}
import doobie.{ConnectionIO, Fragment, Meta}
import org.http4s.implicits.http4sKleisliResponseSyntaxOptionT
import org.http4s.{HttpApp, HttpRoutes}
import org.http4s.server.blaze.BlazeServerBuilder
import doobie._
import doobie.implicits._
import doobie.implicits.javatime._
import doobie.h2._
import org.http4s.Method.GET
import org.http4s.dsl.io._

import scala.concurrent.ExecutionContext.global

object HttpServer extends IOApp{

  override def run(args: List[String]): IO[ExitCode] =
    DbTransactor
      .pooled[IO]
      .use { xa =>
        for {
          // setup db
          _ <- setup().transact(xa)
          // server
          _ <- BlazeServerBuilder[IO](global)
            .withHttpApp(routes(xa).orNotFound)
            .bindHttp(port = 8080, host = "localhost")
            .serve
            .compile
            .drain
        } yield ExitCode.Success
      }

  // setup
  val ddl1 = Fragment.const(createTableAuthorsSql)
  val ddl2 = Fragment.const(createTableBooksSql)
  val dml = Fragment.const(populateDataSql)

  def setup(): ConnectionIO[Unit] =
    for {
      _ <- ddl1.update.run
      _ <- ddl2.update.run
      _ <- dml.update.run
    } yield ()

  final case class SqlCommon[F[_] : Effect](private val xa: Transactor[F]) {

    def getOption[A: Read](f: Fragment) = f.query[A].to[List].transact(xa)

    def delete[A: Read](f: Fragment) = f.update.run.transact(xa)

    def insert[A: Read](f: Fragment) = f.update.run.transact(xa)

    def update[A: Read](f: Fragment) = f.update.run.transact(xa)
  }

  def routes(xa: Transactor[IO]): HttpRoutes[IO] = {
    readDataRoutes(xa)
  }

  def readDataRoutes(xa: Transactor[IO]): HttpRoutes[IO] = {
    HttpRoutes.of[IO] {
      case GET -> Root / "getAllBooks" =>
        for {
          books <- SqlCommon(xa).getOption[Book](DbQuery.readAllBook)
          response <- Ok("asd")
        } yield response
    }
  }


}
