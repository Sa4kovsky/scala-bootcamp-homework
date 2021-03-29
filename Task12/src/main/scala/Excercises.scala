import cats.Functor
import cats.effect.IO
import cats.implicits._

object Excercises {
  trait Applicative[F[_]] extends Functor[F] {
    def map[A,B](fa: F[A])(f: A => B): F[B]

    def unit[A](a: => A): F[A]

    // implement methods using each other
    def map2[A,B,C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = apply(map(fa)(f.curried))(fb)

    def apply[A,B](fab: F[A => B])(fa: F[A]): F[B] = map2(fab, fa)(_(_))

    def sequence[A](fas: List[F[A]]): F[List[A]] = traverse(fas)(f => f)

    def traverse[A,B](as: List[A])(f: A => F[B]): F[List[B]] = sequence(as.map(f))
  }

  trait Monad[M[_]] extends Functor[M] {
    def unit[A](a: => A): M[A]

    // implement methods using each other
    def flatMap[A,B](ma: M[A])(f: A => M[B]): M[B] =  join(map(ma)(f))

    def join[A](mma: M[M[A]]): M[A] = flatMap(mma)(f => f)

    def map[A,B](ma: M[A])(f: A => B): M[B] = flatMap(ma)(a => unit(f(a)))

    def map2[A,B,C](ma: M[A], mb: M[B])(f: (A, B) => C): M[C] = flatMap(ma)(a => map(mb)(b => f(a, b)))
  }
}


object Task {
  // API
  type UserId
  case class User(friends: List[UserId])

  val ids: List[UserId] = ???

  val getUser: UserId => Option[User] = ???

  val getUserAsync: UserId => IO[Option[User]] = ???

  // Task
  val users: List[Option[User]] = ids.map(id => getUser(id))
  val users2: List[User] = users.flatten

  val usersAsync: List[IO[Option[User]]] = ids.map(id => getUserAsync(id))
  val usersAsync2: IO[List[User]] = {
    usersAsync.sequence.map(_.flatten)
  }

  val usersFriendsAsync: List[IO[Option[List[UserId]]]] = ids.map(id => getUserAsync(id).map(_.map(_.friends)))
  val usersFriendsAsync2: IO[List[UserId]] = {
    usersFriendsAsync
      .sequence
      .map(_.flatten.flatten)
  }

  // advanced
  val usersFriendsFriendsAsync: List[IO[Option[List[IO[Option[User]]]]]] = ids.map(id => getUserAsync(id).map(_.map(_.friends.map(id => getUserAsync(id)))))
  val usersFriendsFriendsAsync2: IO[List[User]] = usersFriendsFriendsAsync
    .sequence
    .map(_.flatten.flatten.sequence.map(_.flatten))
    .flatten
}
