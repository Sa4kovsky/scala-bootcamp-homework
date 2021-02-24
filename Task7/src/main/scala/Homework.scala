
object Task1 {
  final case class Money(amount: BigDecimal)

  implicit val moneyOrdering: Ordering[Money] = (x: Money, y: Money) => x.amount.compare(y.amount)
}

object Task2 {
  trait Show[T] {
    def show(entity: T): String
  }

  final case class User(id: String, name: String)

  implicit val showUser: Show[User] = _.name

  implicit class ShowSyntax[T](x: T){
    def show(implicit s: Show[T]) = s.show(x)
  }

  User("1", "Oleg").show
}

object Task3 {
  type Error = String
  trait Parse[T] {
    def parse(entity: String): Either[Error, T]
  }

  final case class User(id: String, name: String)

  implicit class ParseSyntax(x: String) {
    def parse[T](implicit p: Parse[T]): Either[Error, T] = p.parse(x)
  }

  implicit val stringToUser: Parse[User] = x => x.split(',').toList match {
    case id :: name :: Nil => Right(User(id, name))
    case _ => Left("error because it is obviously not a User")
  }

  "lalala".parse[User]
}

object Task4 {
  implicit class EqSyntax[T](x: T) {
    def ===(other: T): Boolean = x == other
  }
}

object TypeclassTask {
  trait Hashable[T] {
    def hash(entity: T): Int
  }

  implicit val hashable: Hashable[String] = _.hashCode

  object HashCode {
    def apply[T](implicit instance: Hashable[T]): Hashable[T] = instance
  }

  implicit class HashableSyntax[T](x: T) {
    def hash(implicit h: Hashable[T]): Int = h.hash(x)
  }
}