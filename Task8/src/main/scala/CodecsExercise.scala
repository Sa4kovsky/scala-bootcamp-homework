import TypeJson._

object CodecsExercise {

  // Exercise 4. Implement Encoder and Decoder for List with any content.
  implicit def listEncoder[A: Encoder]: Encoder[List[A]] = new Encoder[List[A]] {
    override def toJson(a: List[A]): Json = JsonArray(a.map(_.toJson))
  }
  implicit def listDecoder[A: Decoder]: Decoder[List[A]] = new  Decoder[List[A]] {
    override def fromJson(json: Json): Option[List[A]] = {
      json match {
        case JsonArray(value) => Some(value.flatMap(_.as[A]))
      }
    }
  }

  // Exercise 6. Describe Functor
  // 1. Typeclass itself: trait Functor
  // 2. Typeclass Summoner: object Functor
  // 3. Typeclass Ops: implicit class FunctorOps
  // Functor
  trait Functor[F[_]] {
    def fmap[A, B](fa: F[A])(f: A => B): F[B]
  }

  object Functor{
    def apply[F[_]: Functor]: Functor[F] = implicitly[Functor[F]]
  }

  implicit class FunctorOps[F[_]: Functor, A](fa: F[A]) {
    def fmap[B](f: A => B): F[B] = Functor[F].fmap(fa)(f)
  }

  implicit val decoderFunctor = new Functor[Decoder] {
    override def fmap[A, B](fa: Decoder[A])(f: A => B): Decoder[B] = t => fa.fromJson(t).map(f)
  }

  // Exercise 7. Implement Functor for decoder: implicit val decoderFunctor

  // Exercise 8. Describe Contravariant
  // 1. Typeclass itself: trait Contravariant
  // 2. Typeclass Summoner: object Contravariant
  // 3. Typeclass Ops: implicit class ContravariantOps
  trait Conravariant[F[_]]{
    def contrmap[A, B](fa: F[A])(f: B => A): F[B]
  }

  object Conravariant{
    def apply[F[_]: Conravariant]: Conravariant[F] = implicitly[Conravariant[F]]
  }

  implicit class ConravariantOps[F[_]: Conravariant, A](fa: F[A]) {
    def contrmap[B](f: B => A): F[B] = Conravariant[F].contrmap(fa)(f)
  }

  // Exercise 9. Implement Contravariant for encoder: implicit val encoderContravariant
  implicit val encoderContravariant = new Conravariant[Encoder] {
    override def contrmap[A, B](fa: Encoder[A])(f: B => A): Encoder[B] = t => fa.toJson(f(t))
  }

  // Functions Example
  val foo1: String => Int = _.length
  val foo2: Boolean => String = if (_) "100" else "1"

  val fv: Functor[Boolean => *] = new Functor[Boolean => *] {
    override def fmap[A, B](fa: Boolean => A)(f: A => B): Boolean => B = t => f(fa(t))
  }

  val cf: Conravariant[* => Int] = new  Conravariant[* => Int] {
    override def contrmap[A, B](fa: A => Int)(f: B => A): B => Int = t =>  fa(f(t))
  }

  // Exercise 10. Implement Functor and Contravariant for functions:
  // implicit def functionFunctor
  // implicit def functionContravariant
  implicit def functionFunctor: Functor[Function[Boolean, *]] = fv

  implicit def functionContravariant: Conravariant[Function[*, Int]] = cf

  val foo3: Boolean => Int = functionFunctor.fmap(foo2)(foo1)
  val foo4: Boolean => Int = functionContravariant.contrmap(foo1)(foo2)

}
