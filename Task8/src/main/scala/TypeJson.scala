object TypeJson {

  sealed trait Json {
    def /(key: String): JsonResult = this match {
      case JsonObject(value) => JsonResult(value.get(key))
      case _                 => JsonResult(None)
    }
  }

  final case object JsonNull extends Json
  final case class JsonString(value: String) extends Json
  final case class JsonInt(value: Int) extends Json
  final case class JsonArray(value: List[Json]) extends Json
  final case class JsonObject(value: Map[String, Json]) extends Json

  final case class JsonResult(value: Option[Json]) {
    def /(key: String): JsonResult = ???

    def as[A: Decoder]: Option[A] = {
      value.flatMap(Decoder[A].fromJson)
    }
  }

  // Encoder
  trait Encoder[A] {self =>
    def toJson(a: A): Json

    def contrmap[B](f: B => A): Encoder[B] = (a: B) => self.toJson(f(a))
  }

  object Encoder {
    def apply[A: Encoder]: Encoder[A] = implicitly[Encoder[A]]
  }

  implicit class EncoderOps[A: Encoder](a: A) {
    def toJson: Json = Encoder[A].toJson(a)
  }

  // Decoder
  trait Decoder[A] { self =>
    def fromJson(json: Json): Option[A]

    /*  def map[B](f:A => B): Decoder[B] = new Decoder[B] {
        override def fromJson(json: Json): Option[B] = self.fromJson(json).map(f)
      }*/
  }

  object Decoder {
    def apply[A: Decoder]: Decoder[A] = implicitly[Decoder[A]]
  }

  implicit class DecoderOps(json: Json) {
    def as[A: Decoder]: Option[A] = Decoder[A].fromJson(json)
  }

}
