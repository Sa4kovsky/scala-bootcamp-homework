import java.time.{LocalDate, Year}
import java.util.UUID

import doobie._
import doobie.implicits._
import doobie.implicits.javatime._
import doobie.h2._

object implicitMeta
{
  implicit val uuidMeta: Meta[UUID] = Meta[String].timap(UUID.fromString)(_.toString)
  implicit val yearMeta: Meta[Year] = Meta[Int].timap(Year.of)(_.getValue)
}

object DbQuery {
  import implicitMeta._

  val authors: Fragment = fr"SELECT id, name, birthday FROM authors"

  val books: Fragment = fr"SELECT id, author, title, year FROM books"

  val updateAuthor: Fragment = fr"UPDATE authors SET"

  val updateBook: Fragment = fr"UPDATE books SET"

  val deleteAllAuthor: Fragment = fr"DELETE FROM authors"

  val deleteAllBook: Fragment = fr"DELETE FROM books"

  val readAllAuthors = authors

  val readAllBook = books

  def readAuthor(id: UUID)= (authors ++ fr"WHERE id = $id")

  def readBook(id: UUID) = (books ++ fr"WHERE id = $id")

  def insertAuthor(name: String, birthday: LocalDate) = {
    val id = UUID.randomUUID()
    fr"INSERT INTO authors (id, name, birthday) VALUES ($id, $name, $birthday)"
  }

  def insertBook(idAuthor: UUID, title: String, year: Year, genre: String) = {
    val id = UUID.randomUUID()
    fr"INSERT INTO books (id, author, title, year, genre) VALUES ($id, $idAuthor, $title, $year, $genre)"
  }

  def updateAllAuthor(id: UUID, name: String, birthday: LocalDate) =
    (updateAuthor ++ fr"name = $name, birthday = $birthday WHERE id = $id")

  def updateYearOfBook(id: UUID, year: Year) =
    (updateAuthor ++ fr"year = $year where id = $id")

  def deleteBook(id: UUID) =
    (deleteAllBook ++ fr"WHERE id = $id")

  def filterBookOfGenre(genre: String) =
    (authors ++ fr"INNER JOIN authors ON books.author = authors.id WHERE genre  = $genre")
}
