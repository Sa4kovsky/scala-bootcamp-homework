
import cats.effect._
import cats.effect.concurrent.Ref

import scala.io.StdIn
import scala.io.Source
import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext

/*
  Additional assignment:
  1. Read from the console the file path.
    1.1 Use Blocking Thread Pool
    1.2 Check the transmitted data(Error Handling + Validation).
  2. Read from the console the seed.
    2.1 Use Blocking Thread Pool
    2.2 Check the transmitted data(Error Handling + Validation).
  3. Read the data from the file.
  4. Calculate the signature (in parallel if possible).
    4.1 Use Separate Thread Pool(ContextShift)
    4.2 Split text into words
    4.3 Calculate hash for each word
    4.4 Take the minimal hash
    4.5* Repeat the process for n different hash functions.
  5. Save the signature in memory(think about storage).
  6. Terminate the application.
  def javaHash(word: String, seed: Int = 0): Int = {
    var hash = 0
    for (ch <- word.toCharArray)
      hash = 31 * hash + ch.toInt
    hash = hash ^ (hash >> 20) ^ (hash >> 12)
    hash ^ (hash >> 7) ^ (hash >> 4)
  }
  def knuthHash(word: String, constant: Int): Int = {
    var hash = 0
    for (ch <- word.toCharArray)
      hash = ((hash << 5) ^ (hash >> 27)) ^ ch.toInt
    hash % constant
  }
 */
object EffectsHomework2 extends IOApp {
  import Solution._

  override def run(args: List[String]): IO[ExitCode] = {
    val executors = Executors.newCachedThreadPool()
    val ec = ExecutionContext.fromExecutor(executors)
    val blocker = Blocker.liftExecutionContext(ec)
    (for {
      sourceFile <- readFilePath(blocker)
      seed <- readSeed(blocker)
      word <- process(blocker)(sourceFile)
      minHash <- minHash(blocker)(word, seed)
      safe <- IO(Save(minHash, word))
    } yield(safe)).guarantee(IO(executors.shutdown())) as ExitCode.Success
  }
}

object Solution {

  def readFilePath(blocker: Blocker)(implicit contextShift: ContextShift[IO], sync: Sync[IO]): IO[Source] = {
    for {
      _ <- IO(print("Enter file path: "))
      path <- IO(StdIn.readLine())
      source <- blocker.delay {Source.fromFile(path)}.handleErrorWith { _ =>
        IO(println("Invalid file path")) *> readFilePath(blocker)
      }
    } yield source
  }

  def readSeed(blocker: Blocker)(implicit contextShift: ContextShift[IO], sync: Sync[IO]): IO[Int] = {
    for{
      _ <- IO(print("Enter seed: "))
      stdSeed <- IO(StdIn.readLine())
      seed <- blocker.delay{stdSeed.toInt}.handleErrorWith{ _ =>
        IO(println("Invalid seed")) *>  readSeed(blocker)
      }
    } yield seed
  }

  def process(blocker: Blocker)(filePath: Source)(implicit contextShift: ContextShift[IO], sync: Sync[IO]): IO[List[String]] =
    blocker.delay {
      val listText: List[String] = filePath
        .getLines
        .map(_.replaceAll("""[\p{Punct}]""", ""))
        .flatMap(_.split("\\s+"))
        .filter(_ != "")
        .toList

      listText
    }

  def minHash(blocker: Blocker)(word: List[String], seed: Int = 0)(implicit contextShift: ContextShift[IO], sync: Sync[IO]) = {
    for{
      javaHash <- blocker.delay(word.map(word => javaHash(word, seed)).min)
      knuthHash <- blocker.delay(word.map(word => knuthHash(word, seed)).min)
      min <- blocker.delay(javaHash min knuthHash)
      _ <- IO(println(s"MinHash: $min"))
    } yield min
  }

  def javaHash(word: String, seed: Int = 0): Int = {
    var hash = 0
    for (ch <- word.toCharArray)
      hash = 31 * hash + ch.toInt
    hash = hash ^ (hash >> 20) ^ (hash >> 12)
    hash ^ (hash >> 7) ^ (hash >> 4)
  }

  def knuthHash(word: String, constant: Int): Int = {
    var hash = 0
    for (ch <- word.toCharArray)
      hash = ((hash << 5) ^ (hash >> 27)) ^ ch.toInt
    hash % constant
  }
}

case class Save(minHash: Int, list: List[String])
