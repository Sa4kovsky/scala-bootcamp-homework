object QAndAExamples {

  //  Semigroupal
  trait Semigroupal[F[_]] {
    def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]
  }

  object Semigroupal {
    def apply[F[_]: Semigroupal]: Semigroupal[F] = implicitly[Semigroupal[F]]
  }

  implicit class SemigroupalOps[F[_]: Semigroupal, A](x: F[A]) {
    def product[B](y: F[B]): F[(A, B)] = Semigroupal[F].product(x, y)
  }

  implicit class extTuple[F[_]: Functor : Semigroupal, A, B](x: (F[A], F[B])) {
    def mapN[R](f: (A, B) => R): F[R] = x match {
      case (fa, fb) => fa.product(fb).fmap(f.tupled)
    }
  }

  // Functor
  trait Functor[F[_]] {
    def fmap[A, B](fa: F[A])(f: A => B): F[B]
  }

  object Functor {
    def apply[F[_]: Functor]: Functor[F] = implicitly[Functor[F]]
  }

  implicit class FunctorOps[F[_]: Functor, A](fa: F[A]) {
    def fmap[B](f: A => B): F[B] = Functor[F].fmap(fa)(f)
  }

  // 4.4. Implement Semigroupal for Map
  implicit def semigroupalMap[C]: Semigroupal[Map[C, *]] = new Semigroupal[Map[C, *]] {
    override def product[A, B](fa: Map[C, A], fb: Map[C, B]): Map[C, (A, B)] ={
      fa.map({
        case(k, va) => fb.get(k).map(vb => k -> (va, vb))
      }).flatten.toMap
    }
  }

  implicit def functorMapByKey[C]: Functor[Map[C, *]] = new Functor[Map[C, *]] {
    override def fmap[A, B](fa: Map[C, A])(f: A => B): Map[C, B] = {
      fa.view.mapValues(f).toMap
    }
  }

  (Map(1 -> "a", 2 -> "b"), Map(2 -> "c")).mapN(_ + _) == Map(2 -> "bc")

  // 5. Applicative
  trait Applicative[F[_]] extends Semigroupal[F] with Functor[F] {
    def pure[A](x: A): F[A]
  }

  object Applicative {
    def apply[F[_] : Applicative]: Applicative[F] = implicitly[Applicative[F]]
  }

  implicit class ApplicativeOps[F[_] : Applicative, A](x: A) {
    def pure: F[A] = Applicative[F].pure(x)
  }

  // 5.1. Implement Applicative for Option, Either
  implicit def applicativeEither[C]: Applicative[Either[C, *]] = new Applicative[Either[C, *]] {
    override def pure[A](x: A): Either[C, A] = Right(x)

    override def fmap[A, B](fa: Either[C, A])(f: A => B): Either[C, B] = fa.map(f)

    override def product[A, B](fa: Either[C, A], fb: Either[C, B]): Either[C, (A, B)] = {
      for {
        a <- fa
        b <- fb
      } yield (a, b)
    }
  }

  implicit val applicativeOption: Applicative[Option] = new Applicative[Option] {
    def pure[A](x: A): Option[A] = Option(x)

    def fmap[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)

    def product[A, B](fa: Option[A], fb: Option[B]): Option[(A, B)] = {
      for {
        a <- fa
        b <- fb
      } yield (a, b)
    }
  }

  // 5.2. Implement `traverse` for all Applicatives instead of Option
  def traverse[F[_] : Applicative, A, B](as: List[A])(f: A => F[B]): F[List[B]] = {
    val empty = Applicative[F].pure(List.empty[B])
    as.map(f)
      .foldLeft(empty) {
        (acc, x) => (acc, x).mapN((ls, l) => ls :+ l)
      }
  }

  traverse(List(1, 2, 3)) { i =>
    Option.when(i % 2 == 1)(i)
  }.isEmpty

  traverse(List(1, 2, 3)) { i =>
    Option(i + 1)
  } == Option(List(2, 3, 4))

  // 6. Foldable
  trait Foldable[F[_]] {
    def foldLeft[A, B](fa: F[A], b: B)(f: (B, A) => B): B
  }

  object Foldable {
    def apply[T[_]: Foldable]: Foldable[T] = implicitly[Foldable[T]]
  }

  implicit class FoldableOps[F[_]: Foldable, A](fa: F[A]) {
    def foldLeft[B](b: B)(op: (B, A) => B): B = Foldable[F].foldLeft(fa, b)(op)
  }

  // 6.2. Implement Foldable for List
  // Note: we can use here foldLeft from standard library
  implicit def foldLeftFoldable: Foldable[List] = new Foldable[List] {
    override def foldLeft[A, B](fa: List[A], b: B)(f: (B, A) => B): B = fa.foldLeft(b)(f)
  }

  // 6.3. Implement `traverse` for all Foldables instead of List
  def traverse[F[_]: Applicative, G[_]: Functor: Foldable, A, B](l: G[A])(f: A => F[B]): F[G[B]] = {
    l.foldLeft(Applicative[F].pure(???/*G[B]*/)) {
      case (b: F[G[B]], a: A) => (???/*F[G[B]]*/, f(a): F[B]).mapN {
        case (b1 /*G[B]*/, b2 /*B*/) => b1.fmap(bb => )
      }
    }
  }

  def traverseLO[A, B](l: List[A])(f: A => Option[B]): Option[List[B]] = {
    l match {
      case head :: tail => f(head).flatMap(r => traverseLO(tail)(f).map(t => r :: t))
      case Nil => Some(Nil)
    }
  }

  def traverseL[F[_]: Applicative, A, B](l: List[A])(f: A => F[B]): F[List[B]] = {
    l match {
      case head :: tail =>
        (f(head), traverseL(tail)(f)).mapN(_ :: _)
      case Nil => Applicative[F].pure(Nil)
    }
  }

  println(traverseLO(List(1,2,3,4))(x => Some(x)))
  println(traverseLO(List(1,2,3,4))(x => if (x != 2) Some(x) else None))


  println(traverseL(List(1,2,3,4))(x => Some(x).asInstanceOf[Option[Int]]))
  println(traverseL(List(1,2,3,4))(x => if (x != 2) Some(x) else None))

}
