import scala.collection.immutable.ArraySeq
import scala.collection.mutable

object ImplicitsHomework {

  /**
   * Lo and behold! Brand new super-useful collection library for Scala!
   *
   * Our main guest today - [[SuperVipCollections4s.MutableBoundedCache]],
   * a specially crafted, mutable but non-thread-safe (sic!), key-value in-memory cache which bounds the size
   * of the data stored.
   *
   * As the real memory footprint of values on JVM is clouded in mystery, for data size estimation we use
   * a thing called size score. Its calculation rules:
   * - size score of a Byte is 1
   * - Int - 4 (as primitive JVM int consists of 4 bytes)
   * - Long - 8
   * - Char - 2 (one UTF-16 symbol is 2 bytes)
   * - String - 12 (supposedly the size of the JVM object header) + length * size score of Char
   * - score for any case class is 12 (again our folk wisdom about JVM object layout) + sum of scores of all
   * the fields
   * - score for any sequence (Array[T], List[T], Vector[T]) is
   * 12 (our old friend object header) + sum of scores of all elements
   * - score for any Map[K, V] is 12 + sum of scores of all keys + sum of scores of all values
   */
  object SuperVipCollections4s {
    type SizeScore = Int

    trait GetSizeScore[T] {
      def apply(value: T): SizeScore
    }

    object GetSizeScore {
      def apply[A: GetSizeScore]: GetSizeScore[A] = implicitly[GetSizeScore[A]]
    }

    object syntax {

      implicit class GetSizeScoreOps[T: GetSizeScore](inner: T) {
        def sizeScore: SizeScore = GetSizeScore[T].apply(inner) //implement the syntax!
      }

      object FixedSizeScore {
        val sizeScoreByte = 1
        val sizeScoreChar = 2
        val sizeScoreInt = 4
        val sizeScoreLong = 8

        val sizeObjectHeader = 12
      }

    }

    /**
     * Mutable key-value cache which limits the size score of the data scored.
     *
     * The size score of the data is sum of size scores of all keys + sum of size scores of all values.
     * If upon insertion the total score gets over [[maxSizeScore]], the oldest KV-pairs
     * (in the insertion order) should be evicted. If even with the eviction of all the existing elements,
     * the KV-pair can't be added without violating [[maxSizeScore]] - the behaviour is undefined.
     *
     * @param maxSizeScore max size score for the stored data
     * @tparam K key type
     * @tparam V value type
     */
    final class MutableBoundedCache[K: GetSizeScore, V: GetSizeScore](maxSizeScore: SizeScore) {

      import syntax._

      /*
      mutable.LinkedHashMap is a mutable map container which preserves insertion order - this might be useful!
       */
      private val map = mutable.LinkedHashMap.empty[K, V]

      private implicit val mapGetSizeScore: GetSizeScore[mutable.LinkedHashMap[K, V]] = new GetSizeScore[mutable.LinkedHashMap[K, V]] {
        override def apply(value: mutable.LinkedHashMap[K, V]): SizeScore = value.map {
          case (k, v) => k.sizeScore + v.sizeScore
        }.sum
      }

      def put(key: K, value: V): Unit = {
        while (maxSizeScore < map.sizeScore + key.sizeScore + value.sizeScore) {
          val (k, v) = map.head
          map.remove(k)
        }
        map.update(key, value)
      }

      def get(key: K): Option[V] = map.get(key)
    }

    /**
     * Cool custom immutable multi-map collection - does not extend the standard library collection types
     * (yes, this is a feature)
     */
    final case class PackedMultiMap[K, +V](inner: ArraySeq[(K, V)])

    object PackedMultiMap {
      def empty[K, V]: PackedMultiMap[K, V] = PackedMultiMap()

      def apply[K, V](values: (K, V)*): PackedMultiMap[K, V] = PackedMultiMap(inner = ArraySeq(values: _*))
    }

    /**
     * Type-class allowing us to iterate over different "collection-like" types with one type arg
     */
    trait Iterate[-F[_]] {
      def iterator[T](f: F[T]): Iterator[T]
    }

    /**
     * Same as [[Iterate]] but for collections containing 2 types of values (think Map's and like)
     */
    trait Iterate2[-F[_, _]] {
      def iterator1[T, S](f: F[T, S]): Iterator[T]

      def iterator2[T, S](f: F[T, S]): Iterator[S]
    }

    object instances {

      import syntax._
      import FixedSizeScore._

      implicit val iterableOnceIterate: Iterate[Iterable] = new Iterate[Iterable] {
        override def iterator[T](f: Iterable[T]): Iterator[T] = f.iterator
      }

      //Array is not an Iterable in Scala 2.13 but we still might abstract over iteration logic for both!
      implicit val arrayIterate: Iterate[Array] = new Iterate[Array] {
        override def iterator[T](f: Array[T]): Iterator[T] = f.iterator
      }

      implicit val mapIterate: Iterate2[Map] = new Iterate2[Map] {
        override def iterator1[T, S](f: Map[T, S]): Iterator[T] = f.keys.iterator

        override def iterator2[T, S](f: Map[T, S]): Iterator[S] = f.values.iterator
      }

      implicit val mapPackedMultiMapIterate: Iterate2[PackedMultiMap] = new Iterate2[PackedMultiMap] {
        override def iterator1[T, S](f: PackedMultiMap[T, S]): Iterator[T] = f.inner.toMap.keysIterator

        override def iterator2[T, S](f: PackedMultiMap[T, S]): Iterator[S] = f.inner.toMap.valuesIterator
      }


      //Provide Iterate2 instances for Map and PackedMultiMap!
      //if the code doesn't compile while you think it should - sometimes full rebuild helps!

      /*
      replace this big guy with proper implicit instances for types:
      - Byte, Char, Int, Long
      - String
      - Array[T], List[T], Vector[T], Map[K,V], PackedMultiMap[K,V]
        - points to karma if you provide those in a generic way
        (Iterate and Iterate2 type-classes might be helpful!)

      If you struggle with writing generic instances for Iterate and Iterate2, start by writing instances for
      List and other collections and then replace those with generic instances.
       */

      implicit val byteSizeScore: GetSizeScore[Byte] = new GetSizeScore[Byte] {
        override def apply(value: Byte): SizeScore = sizeScoreByte
      }

      implicit val charSizeScore: GetSizeScore[Char] = new GetSizeScore[Char] {
        override def apply(value: Char): SizeScore = sizeScoreChar
      }

      implicit val intSizeScore: GetSizeScore[Int] = new GetSizeScore[Int] {
        override def apply(value: Int): SizeScore = sizeScoreInt
      }

      implicit val longSizeScore: GetSizeScore[Long] = new GetSizeScore[Long] {
        override def apply(value: Long): SizeScore = sizeScoreLong
      }

      implicit val stringSizeScore: GetSizeScore[String] = new GetSizeScore[String] {
        override def apply(value: String): SizeScore = sizeObjectHeader + value.length * sizeScoreChar
      }

      implicit def arraySizeScore[T: GetSizeScore]: GetSizeScore[Array[T]] = new GetSizeScore[Array[T]] {
        override def apply(value: Array[T]): SizeScore = sizeObjectHeader + value.map(_.sizeScore).sum
      }

      implicit def listSizeScore[T: GetSizeScore]: GetSizeScore[List[T]] = new GetSizeScore[List[T]] {
        override def apply(value: List[T]): SizeScore = sizeObjectHeader + value.map(_.sizeScore).sum
      }

      implicit def vectorSizeScore[T: GetSizeScore]: GetSizeScore[Vector[T]] = new GetSizeScore[Vector[T]] {
        override def apply(value: Vector[T]): SizeScore = sizeObjectHeader + value.map(_.sizeScore).sum
      }

      implicit def mapSizeScore[T: GetSizeScore, V: GetSizeScore]: GetSizeScore[Map[T, V]] = new GetSizeScore[Map[T, V]] {
        override def apply(value: Map[T, V]): SizeScore = {
          (for {
            (k, v) <- value.view
          } yield k.sizeScore + v.sizeScore).sum + sizeObjectHeader


        }
      }

      implicit def packedMapSizeScore[T: GetSizeScore, V: GetSizeScore]: GetSizeScore[PackedMultiMap[T, V]] = new GetSizeScore[PackedMultiMap[T, V]] {
        override def apply(value: PackedMultiMap[T, V]): SizeScore = {
          (for {
            (k, v) <- value.inner
          } yield k.sizeScore + v.sizeScore).sum + sizeObjectHeader
        }
      }
    }
  }

  /*
  Time to bring some business value!
  #GoodVibes #ThrowbackThursday #NoFilter #squadgoals
   */
  object MyTwitter {

    import SuperVipCollections4s._
    import instances._
    import syntax._
    import FixedSizeScore._

    final case class Twit(id: Long, userId: Int, hashTags: Vector[String], attributes: PackedMultiMap[String, String], fbiNotes: List[FbiNote])

    final case class FbiNote(month: String, favouriteChar: Char, watchedPewDiePieTimes: Long)

    trait TwitCache {
      def put(twit: Twit): Unit

      def get(id: Long): Option[Twit]
    }

    implicit val fbiNoteSizeScore: GetSizeScore[FbiNote] = new GetSizeScore[FbiNote] {
      override def apply(value: FbiNote): SizeScore = value.favouriteChar.sizeScore + value.month.sizeScore + value.watchedPewDiePieTimes.sizeScore + sizeObjectHeader
    }

    implicit val twitSizeScore: GetSizeScore[Twit] = new GetSizeScore[Twit] {
      override def apply(value: Twit): SizeScore = value.fbiNotes.sizeScore + value.userId.sizeScore + value.attributes.sizeScore + value.hashTags.sizeScore + value.id.sizeScore + sizeObjectHeader
    }

    /*
    Return an implementation based on MutableBoundedCache[Long, Twit]
     */
    def createTwitCache(maxSizeScore: SizeScore): TwitCache = new TwitCache {

      import instances._

      val cache = new MutableBoundedCache[Long, Twit](maxSizeScore)

      override def put(twit: Twit): Unit = cache.put(twit.id, twit)

      override def get(id: Long): Option[Twit] = cache.get(id)
    }
  }
}

