package scala
package collection
package immutable

/** This class implements an immutable map that preserves order using
  * a hash map for the key to value mapping to provide efficient lookup,
  * and a tree for the ordering of the keys to provide efficient
  * insertion/modification order traversal and destructuring.
  *
  * By default insertion order (`OrderedMapX.OrderBy.Insertion`)
  * is used, but modification order (`OrderedMapX.OrderBy.Modification`)
  * can be used instead if so specified at creation.
  *
  * The `orderingBy(orderBy: OrderedMapX.OrderBy): OrderedMapX[K, V]` method
  * can be used to switch to the specified ordering for the returned map.
  *
  * A key can be manually refreshed (i.e. placed at the end) via the
  * `refresh(key: K): OrderedMapX[K, V]` method (regardless of the ordering in
  * use).

  * Internally, an ordinal counter is increased for each insertion/modification
  * and then the current ordinal is used as key in the tree map. After 2^32^
  * insertions/modifications the entire map is copied (thus resetting the ordinal
  * counter).
  *
  *
  *  @tparam K the type of the keys contained in this map.
  *  @tparam V the type of the values associated with the keys in this map.
  *
  * @author Odd Möller
  * @version 2.13
  * @since 2.13
  * @define coll immutable ordered map
  * @define Coll `immutable.OrderedMapX`
  */
final class OrderedMapX[K, +V] private (
    private val ordering: OrderedMapX.Ordering[K],
    private val mapping: OrderedMapX.Mapping[K, V],
    private val ordinal: Int,
    val orderedBy: OrderedMapX.OrderBy)
  extends AbstractMap[K, V]
    with SeqMap[K, V]
    with MapOps[K, V, OrderedMapX, OrderedMapX[K, V]]
    with StrictOptimizedIterableOps[(K, V), Iterable, OrderedMapX[K, V]]
    with StrictOptimizedMapOps[K, V, OrderedMapX, OrderedMapX[K, V]] {

  import OrderedMapX._

  override protected[this] def className: String = "OrderedMapX"

  override def mapFactory: MapFactory[OrderedMapX] = OrderedMapX

  override val size = mapping.size

  override def knownSize: Int = size

  override def isEmpty = size == 0

  def orderingBy(orderBy: OrderBy): OrderedMapX[K, V] = {
    if (orderBy == this.orderedBy) this
    else new OrderedMapX(ordering, mapping, ordinal, orderBy)
  }

  def updated[V1 >: V](key: K, value: V1): OrderedMapX[K, V1] = {
    mapping.get(key) match {
      case e if ordinal == -1 && (orderedBy == OrderBy.Modification || e.isEmpty) ⇒
        // Reinsert into fresh instance to restart ordinal counting, expensive but only done after 2^32 updates.
        OrderedMapX.empty[K, V](orderedBy) ++ this + (key → value)
      case Some((o, _)) if orderedBy == OrderBy.Insertion =>
        new OrderedMapX(
          ordering.incl(o, key),
          mapping.updated[(Int, V1)](key, (o, value)),
          o,
          orderedBy)
      case Some((o, _)) =>
        val o1 = if (ordinal == Int.MaxValue) Int.MinValue else ordinal + 1
        new OrderedMapX(
          ordering.excl(o).incl(o1, key),
          mapping.updated[(Int, V1)](key, (o1, value)),
          o1,
          orderedBy)
      case None ⇒
        val o1 = ordinal + 1
        new OrderedMapX(
          ordering.incl(o1, key),
          mapping.updated[(Int, V1)](key, (o1, value)),
          o1,
          orderedBy)
    }
  }

  def remove(key: K): OrderedMapX[K, V] = {
    mapping.get(key) match {
      case Some((o, _)) ⇒
        new OrderedMapX(
          ordering.excl(o),
          mapping.remove(key),
          ordinal,
          orderedBy)
      case None ⇒
        this
    }
  }

  def refresh(key: K): OrderedMapX[K, V] = {
    mapping.get(key) match {
      case Some((o, _)) ⇒
        val o1 = ordinal + 1
        new OrderedMapX(
          ordering.excl(o).incl(o1, key),
          mapping,
          o1,
          orderedBy)
      case None ⇒
        this
    }
  }

  override def contains(key: K): Boolean = mapping.contains(key)

  override def head: (K, V) = binding(ordering.head)

  override def headOption = ordering.headOption.map(binding)

  override def last: (K, V) = binding(ordering.last)

  override def lastOption: Option[(K, V)] = ordering.lastOption.map(binding)

  override def tail: OrderedMapX[K, V] = {
    val (head, tail) = ordering.headTail
    new OrderedMapX(tail, mapping.remove(head), ordinal, orderedBy)
  }

  override def init: OrderedMapX[K, V] = {
    val (init, last) = ordering.initLast
    new OrderedMapX(init, mapping.remove(last), ordinal, orderedBy)
  }

  def get(key: K): Option[V] = mapping.get(key).map(value)

  def iterator: Iterator[(K, V)] = new AbstractIterator[(K, V)] {
    private[this] val iter = ordering.iterator

    override def hasNext: Boolean = iter.hasNext

    override def next(): (K, V) = binding(iter.next())
  }

  override def keysIterator: Iterator[K] = new AbstractIterator[K] {
    private[this] val iter = ordering.iterator

    override def hasNext: Boolean = iter.hasNext

    override def next(): K = iter.next()
  }

  override def valuesIterator: Iterator[V] = new AbstractIterator[V] {
    private[this] val iter = ordering.iterator

    override def hasNext: Boolean = iter.hasNext

    override def next(): V = value(binding(iter.next()))
  }

  override def concat[V1 >: V](suffix: IterableOnce[(K, V1)]): OrderedMapX[K, V1] = {
    var result: OrderedMapX[K, V1] = this
    val iter = suffix.iterator
    while (iter.hasNext) {
      val (k, v) = iter.next()
      result = result.updated(k, v)
    }
    result
  }

  override def map[K2, V2](f: ((K, V)) => (K2, V2)): OrderedMapX[K2, V2] = {
    var ong: Ordering[K2] = Ordering.empty
    val mng: Mapping[K2, V2] = mapping.map {
      case (k, (o, v)) =>
        val (k2, v2) = f((k, v))
        ong = ong.incl(o, k2)
        (k2, (o, v2))
    }
    new OrderedMapX[K2, V2](ong, mng, ordinal, orderedBy)
  }

  override def flatMap[K2, V2](f: ((K, V)) => IterableOnce[(K2, V2)]): OrderedMapX[K2, V2] = {
    var ong: Ordering[K2] = Ordering.empty
    val mng: Mapping[K2, V2] = mapping.flatMap {
      case (k, (o, v)) =>
        f((k, v)).iterator.reduceLeftOption((_, e) => e).map {
          case (k2, v2) =>
            ong = ong.incl(o, k2)
            (k2, (o, v2))
        }
    }
    new OrderedMapX[K2, V2](ong, mng, ordinal, orderedBy)
  }

  override def collect[K2, V2](pf: PartialFunction[(K, V), (K2, V2)]): OrderedMapX[K2, V2] = {
    var ong: Ordering[K2] = Ordering.empty
    val mng: Mapping[K2, V2] = mapping.collect {
      case (k, (o, v)) if pf.isDefinedAt((k, v)) =>
        val (k2, v2) = pf((k, v))
        ong = ong.incl(o, k2)
        (k2, (o, v2))
    }
    new OrderedMapX[K2, V2](ong, mng, ordinal, orderedBy)
  }

  @`inline` private[this] def value(p: (_, V)) = p._2
  @`inline` private[this] def binding(k: K) = mapping(k).copy(_1 = k)
}
object OrderedMapX extends MapFactory[OrderedMapX] {
  sealed trait OrderBy
  final object OrderBy {
    final case object Insertion extends OrderBy
    final case object Modification extends OrderBy
  }

  val Empty = new OrderedMapX[Nothing, Nothing](Ordering.empty, HashMap.empty, 0, OrderBy.Insertion)
  def empty[K, V]: OrderedMapX[K, V] = empty(OrderBy.Insertion)
  def empty[K, V](orderBy: OrderBy): OrderedMapX[K, V] = Empty.asInstanceOf[OrderedMapX[K, V]]

  def from[K, V](it: collection.IterableOnce[(K, V)]): OrderedMapX[K, V] =
    it match {
      case vm: OrderedMapX[K, V] => vm
      case _                   => (newBuilder[K, V] ++= it).result()
    }

  def newBuilder[K, V]: mutable.Builder[(K, V), OrderedMapX[K, V]] = newBuilder(OrderBy.Insertion)

  def newBuilder[K, V](orderBy: OrderBy): mutable.Builder[(K, V), OrderedMapX[K, V]] =
    new mutable.ImmutableBuilder[(K, V), OrderedMapX[K, V]](empty(orderBy)) {
      def addOne(elem: (K, V)): this.type = { elems = elems + elem; this }
    }

  private type Mapping[K, +V] = HashMap[K, (Int, V)]
  private val Mapping = HashMap

  /* The ordering implementation below is an adapted version of immutable.IntMap. */
  private object Ordering {
    import scala.collection.generic.BitOperations.Int._
    //import scala.language.implicitConversions

    type Ordinal = Int

    def empty[T] : Ordering[T] = Zero

    def apply[T](elems: (Int, T)*): Ordering[T] =
      elems.foldLeft(empty[T])((x, y) => x.incl(y._1, y._2))

    // Iterator over a non-empty Ordering.
    final class Iterator[+V](it: Ordering[V]) {
      // Basically this uses a simple stack to emulate conversion over the tree. However
      // because we know that Ints are at least 32 bits we can have at most 32 Bins and
      // one Tip sitting on the tree at any point. Therefore we know the maximum stack
      // depth is 33
      private[this] var index = 0
      private[this] val buffer = new Array[AnyRef](33)

      private[this] def pop = {
        index -= 1
        buffer(index).asInstanceOf[Ordering[V]]
      }

      private[this] def push[V2 >: V](x: Ordering[V2]): Unit = {
        buffer(index) = x.asInstanceOf[AnyRef]
        index += 1
      }

      if (it != Zero) push(it)

      def hasNext = index != 0
      def next(): V =
        pop match {
          case Bin(_,_, Tip(_, v), right) =>
            push(right)
            v
          case Bin(_, _, left, right) =>
            push(right)
            push(left)
            next()
          case Tip(_, v) => v
          // This should never happen. We don't allow Ordering.Zero in subtrees of the Ordering
          // and don't return an Ordering.Iterator for Ordering.Zero.
          case Zero => throw new IllegalStateException("empty subtree not allowed")
        }
    }

    object Iterator {
      val Empty = new Iterator[Nothing](Ordering.empty[Nothing])
      def empty[V]: Iterator[V] = Empty.asInstanceOf[Iterator[V]]
    }

    private final case object Zero extends Ordering[Nothing] {
      // Important! Without this equals method in place, an infinite
      // loop from Map.equals => size => pattern-match-on-Nil => equals
      // develops.  Case objects and custom equality don't mix without
      // careful handling.
      override def equals(that : Any): Boolean = that match {
        case _: this.type => true
        case _: Ordering[_] => false // The only empty Orderings are eq Nil
        case _ => super.equals(that)
      }
    }

    private final case class Tip[+T](ord: Int, value: T) extends Ordering[T] {
      def withValue[S](s: S) =
        if (s.asInstanceOf[AnyRef] eq value.asInstanceOf[AnyRef]) this.asInstanceOf[Tip[S]]
        else Tip(ord, s)
    }

    private final case class Bin[+T](prefix: Int, mask: Int, left: Ordering[T], right: Ordering[T]) extends Ordering[T] {
      def bin[S](left: Ordering[S], right: Ordering[S]): Ordering[S] = {
        if ((this.left eq left) && (this.right eq right)) this.asInstanceOf[Bin[S]]
        else Bin[S](prefix, mask, left, right)
      }
    }

    private def branchMask(i: Int, j: Int) = highestOneBit(i ^ j)

    private def join[T](p1: Int, t1: Ordering[T], p2: Int, t2: Ordering[T]): Ordering[T] = {
      val m = branchMask(p1, p2)
      val p = mask(p1, m)
      if (zero(p1, m)) Bin(p, m, t1, t2)
      else Bin(p, m, t2, t1)
    }

    private def bin[T](prefix: Int, mask: Int, left: Ordering[T], right: Ordering[T]): Ordering[T] = (left, right) match {
      case (l, Zero) => l
      case (Zero, r) => r
      case (l, r) => Bin(prefix, mask, l, r)
    }
  }

  private sealed abstract class Ordering[+T] {
    import Ordering._
    import scala.annotation.tailrec
    import scala.collection.generic.BitOperations.Int._

    @tailrec
    final def head: T = this match {
      case Bin(_, _, l, _) => l.head
      case Tip(k, v) => v
      case Zero => throw new NoSuchElementException("head of empty map")
    }

    @tailrec
    final def headOption: Option[T] = this match {
      case Bin(_, _, l, _) => l.headOption
      case Tip(_, v) => Some(v)
      case Zero => None
    }

    @tailrec
    final def last: T = this match {
      case Bin(_, _, _, r) => r.last
      case Tip(_, v) => v
      case Zero => throw new NoSuchElementException("last of empty map")
    }

    @tailrec
    final def lastOption: Option[T] = this match {
      case Bin(_, _, _, r) => r.lastOption
      case Tip(_, v) => Some(v)
      case Zero => None
    }

    final def tail: Ordering[T] = this match {
      case Bin(p, m, l, r) => bin(p, m, l.tail, r)
      case Tip(_, _) => Zero
      case Zero => throw new NoSuchElementException("tail of empty map")
    }

    final def headTail: (T, Ordering[T]) = this match {
      case Bin(p, m, l, r) =>
        val (head, tail) = l.headTail
        (head, bin(p, m, tail, r))
      case Tip(_, v) => (v, Zero)
      case Zero => throw new NoSuchElementException("init of empty map")
    }

    final def init: Ordering[T] = this match {
      case Bin(p, m, l, r) =>
        bin(p, m, l, r.init)
      case Tip(_, _) => Zero
      case Zero => throw new NoSuchElementException("init of empty map")
    }

    final def initLast: (Ordering[T], T) = this match {
      case Bin(p, m, l, r) =>
        val (init, last) = r.initLast
        (bin(p, m, l, init), last)
      case Tip(_, v) => (Zero, v)
      case Zero => throw new NoSuchElementException("init of empty map")
    }

    final def iterator: Iterator[T] = this match {
      case Zero => Iterator.empty
      case _ => new Iterator(this)
    }

    final def incl[S >: T](ordinal: Int, value: S): Ordering[S] = this match {
      case Bin(p, m, l, r) =>
        if (!hasMatch(ordinal, p, m)) join(ordinal, Tip(ordinal, value), p, this)
        else if (zero(ordinal, m)) Bin(p, m, l.incl(ordinal, value), r)
        else Bin(p, m, l, r.incl(ordinal, value))
      case Tip(o, _) =>
        if (ordinal == o) Tip(ordinal, value)
        else join(ordinal, Tip(ordinal, value), o, this)
      case Zero => Tip(ordinal, value)
    }

    final def excl(ordinal: Int): Ordering[T] = this match {
      case Bin(p, m, l, r) =>
        if (!hasMatch(ordinal, p, m)) this
        else if (zero(ordinal, m)) bin(p, m, l.excl(ordinal), r)
        else bin(p, m, l, r.excl(ordinal))
      case Tip(o, _) =>
        if (ordinal == o) Zero
        else this
      case Zero => Zero
    }
  }
}