import cats.implicits._
import cats.Monad
import cats.effect.concurrent.Ref
import cats.effect.{Clock, Concurrent, ExitCode, Fiber, IO, IOApp, Timer}

import java.security.Timestamp
import scala.concurrent.duration._

/*
 * Please implement a Cache which allows concurrent access.
 *
 * Tip: checking expiration could be represented as some infinite process somewhere in background
 *
 * Cached items should have an expiration timestamp after which they are evicted.
 */
object SharedStateHomework extends IOApp {

  type CacheMap[K, V]         = Map[K, (Timestamp, V)]
  type CacheState[F[_], K, V] = Ref[F, CacheMap[K, V]]

  trait Cache[F[_], K, V] {
    def get(key: K): F[Option[V]]

    def put(key: K, value: V): F[Unit]
  }

  class RefCache[F[_] : Clock : Monad, K, V](
                                              state: Ref[F, Map[K, (Long, V)]],
                                              expiresIn: FiniteDuration
                                            ) extends Cache[F, K, V] {

    def get(key: K): F[Option[V]] = {
      state.get.map { map =>
        map.get(key).map { case (_, v) => v }
      }
    }

    def put(key: K, value: V): F[Unit] = for {
      time <- Clock[F].realTime(MILLISECONDS)
      _ <- state.update(_.updated(key, (time + expiresIn.toMillis, value)))
    } yield ()

    def expired: F[Unit] = for {
      realTime <- Clock[F].realTime(MILLISECONDS)
      _ <- state.update { map => map.filter { case (_, (time, _)) => time < realTime }
      }
    } yield ()

  }

  object Cache {
    def of[F[_] : Clock, K, V](
                                expiresIn: FiniteDuration,
                                checkOnExpirationsEvery: FiniteDuration
                              )(implicit T: Timer[F], C: Concurrent[F]): F[Cache[F, K, V]] = {
      val cache = Ref.of[F, Map[K, (Long, V)]](Map()).map { state => new RefCache(state, expiresIn) }
      def evict(cache: RefCache[F, K, V]): F[Unit] = for {
        _ <- T.sleep(checkOnExpirationsEvery)
        _ <- cache.expired
        _ <- evict(cache)
      } yield ()

      for {
        c <- cache
        _ <- Concurrent[F].start(evict(c))
      } yield c
    }

  }

  override def run(args: List[String]): IO[ExitCode] = {

    for {
      cache <- Cache.of[IO, Int, String](10.seconds, 4.seconds)
      _ <- cache.put(1, "Hello")
      _ <- cache.put(2, "World")
      _ <- cache.get(1).flatMap(s => IO {
        println(s"first key $s")
      })
      _ <- cache.get(2).flatMap(s => IO {
        println(s"second key $s")
      })
      _ <- IO.sleep(12.seconds)
      _ <- cache.get(1).flatMap(s => IO {
        println(s"first key $s")
      })
      _ <- cache.get(2).flatMap(s => IO {
        println(s"second key $s")
      })
      _ <- IO.sleep(12.seconds)
      _ <- cache.get(1).flatMap(s => IO {
        println(s"first key $s")
      })
      _ <- cache.get(2).flatMap(s => IO {
        println(s"second key $s")
      })
    } yield ExitCode.Success
  }
}

