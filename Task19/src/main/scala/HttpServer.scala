import DbCommon._
import DbQuery._
import cats.data.Validated
import cats.effect.{Bracket, Effect, ExitCode, IO, IOApp}
import cats.implicits.toSemigroupKOps
import doobie.{ConnectionIO, Fragment, Meta}
import org.http4s.implicits.http4sKleisliResponseSyntaxOptionT
import org.http4s.{HttpApp, HttpRoutes, ParseFailure, QueryParamDecoder}
import org.http4s.server.blaze.BlazeServerBuilder
import doobie._
import doobie.implicits._
import doobie.implicits.javatime._
import doobie.h2._
import org.http4s.Method.{GET}
import org.http4s.dsl.io._
import io.circe.generic.auto._
import org.http4s.circe.CirceEntityCodec.{circeEntityDecoder, circeEntityEncoder}

import java.util.UUID
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

    def delete(f: Fragment) = f.update.run.transact(xa)

    def insert(f: Fragment) = f.update.run.transact(xa)

    def update(f: Fragment) = f.update.run.transact(xa)
  }

  def routes(xa: Transactor[IO]): HttpRoutes[IO] = {
    readDataRoutes(xa) <+> deleteDataRoutes(xa) <+> updateDataRoutes(xa) <+> insertDataRoutes(xa)
  }

  implicit val UUIDDecoder: QueryParamDecoder[UUID] = { param =>
    Validated
      .catchNonFatal(UUID.fromString(param.value))
      .leftMap(t => ParseFailure(s"Failed", t.getMessage))
      .toValidatedNel
  }

  object UUIDQuery extends QueryParamDecoderMatcher[UUID]("id")
  object GenreQuery extends QueryParamDecoderMatcher[String](name = "genre")

  def readDataRoutes(xa: Transactor[IO]): HttpRoutes[IO] = {
    HttpRoutes.of[IO] {
      case GET -> Root / "getAllBooks" =>
        for {
          books <- SqlCommon(xa).getOption[Book](readAllBook)
          response <- Ok(books)
        } yield response
    }

    HttpRoutes.of[IO] {
      case GET -> Root / "getAllAuthor" =>
        for {
          author <- SqlCommon(xa).getOption[Author](readAllAuthors)
          response <- Ok(author)
        } yield response
    }

    HttpRoutes.of[IO] {
      case GET -> Root / "getAuthor" :? UUIDQuery(id) =>
        for {
          author <- SqlCommon(xa).getOption[Author](readAuthor(id))
          response <- Ok(author)
        } yield response
    }

    HttpRoutes.of[IO] {
      case GET -> Root / "filterBookOfGenre" :? GenreQuery(genre) =>
        for {
          books <- SqlCommon(xa).getOption[BookWithAuthor](filterBookOfGenre(genre))
          response <- Ok(books)
        } yield response
    }
  }

  def updateDataRoutes(xa: Transactor[IO]): HttpRoutes[IO] = {
    HttpRoutes.of[IO] {
      case req@POST -> Root / "updateAllBook" => for {
        x <- req.as[Book]
        _ <- SqlCommon(xa).update(updateYearOfBook(x.id, x.year) )
        response <- Ok(s"${x.title} was updated")
      } yield response

      case req@POST -> Root / "updateAllAuthor" => for {
        x <- req.as[Author]
        _ <- SqlCommon(xa).update(updateAllAuthor(x.id, x.name, x.birthday))
        response <- Ok(s"${x.name} was updated")
      } yield response
    }
  }

  def insertDataRoutes(xa: Transactor[IO]): HttpRoutes[IO] = {
    HttpRoutes.of[IO] {
      case req@POST -> Root / "updateAllBook" => for {
          x <- req.as[Book]
          _ <- SqlCommon(xa).insert(insertBook(x.authorId, x.title, x.year, x.genre))
          response <- Ok(s"${x.title} was added")
        } yield response

      case req@POST -> Root / "updateAllAuthor" => for {
        x <- req.as[Author]
        _ <- SqlCommon(xa).insert(insertAuthor(x.name, x.birthday))
        response <- Ok(s"${x.name} was added")
      } yield response
    }
  }

  def deleteDataRoutes(xa: Transactor[IO]): HttpRoutes[IO] = {
    HttpRoutes.of[IO] {
      case DELETE -> Root / "deleteAllBook" => for {
       x <- SqlCommon(xa).delete(deleteAllBook)
       response <- if(x > 0) Ok("All books was deleted") else Ok("Error")
      } yield response

      case DELETE -> Root / "deleteAllAuthor" => for {
        x <- SqlCommon(xa).delete(deleteAllAuthor)
        response <- if(x > 0) Ok("All authors was deleted") else Ok("Error")
      } yield response

      case DELETE -> Root / "deleteBook" :? UUIDQuery(id) => for {
        x <- SqlCommon(xa).delete(deleteBook(id))
        response <- if(x > 0) Ok(s"Book (id: $id) was deleted") else Ok("Error")
      } yield response
    }
  }
}
