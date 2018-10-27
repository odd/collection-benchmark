package ops.immutable

import java.util.concurrent.TimeUnit
import scala.annotation.tailrec
import scala.collection.immutable.NumericRange
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

@BenchmarkMode(scala.Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@Fork(1)
@Warmup(iterations = 8)
@Measurement(iterations = 8)
@State(Scope.Benchmark)
class LazyListBenchmark {
  //@Param(scala.Array("0", "1", "2", "3", "4", "7", "8", "15", "16", "17", "39", "282", "4096", "131070", "7312102"))
  @Param(scala.Array(/*"0", */"1"/*, "2", "3", "4"*/, "7"/*, "8"*//*, "15"*//*, "16"*//*, "17"*//*, "33"*//*, "282"*/, "4096"/*, "131070"*//*, "7312102"*/))
  var size: Int = _

  val random = new scala.util.Random(19740115L)
  val empty: LazyList[Long] = LazyList.empty[Long]
  var xs: LazyList[Long] = _
  var ys: LazyList[Long] = _
  var zs: LazyList[Long] = _
  var zipped: LazyList[(Long, Long)] = _
  var randomIndices: scala.Array[Int] = _
  def fresh(n: Int) = LazyList(1L to n.toLong: _*)
  def freshBuilder() = LazyList.newBuilder[Long]

  @Setup(Level.Trial)
  def initTrial(): Unit = {
    xs = fresh(size)
    ys = LazyList(1L to size.toLong / 2L: _*) #::: LazyList((size.toLong / 2L) to 1L by -1L: _*)
    zs = fresh((size / 1000) max 2).map(-_)
    zipped = xs.map(x => (x, x))
    if (size > 0) {
      randomIndices = scala.Array.fill(1000)(random.nextInt(size))
    }
  }

  @Benchmark
  def create_apply(bh: Blackhole): Unit = bh.consume(fresh(size))

  @Benchmark
  def create_build(bh: Blackhole): Unit = {
    var i = 0L
    val builder = freshBuilder()
    builder.sizeHint(size)
    while (i < size) {
      builder += i
      i += 1
    }
    bh.consume(builder.result())
  }

  @Benchmark
  def create_buildRange(bh: Blackhole): Unit = {
    var i = 0L
    val builder = freshBuilder()
    //rintln("# builder: " + builder.getClass.getName)
    builder.sizeHint(size * 10)
    while (i < size) {
      builder ++= xs.iterableFactory.range(i - 10, i)
      i += 1
    }
    bh.consume(builder.result())
  }

  @Benchmark
  def create_buildNumericRange(bh: Blackhole): Unit = {
    var i = 0L
    val builder = freshBuilder()
    builder.sizeHint(size * 10)
    while (i < size) {
      builder ++= NumericRange(i - 10, i, 1)
      i += 1
    }
    bh.consume(builder.result())
  }

  @Benchmark
  def expand_foldAppend(bh: Blackhole): Unit = {
    val ys = (1L to size).foldLeft(empty) {
      case (acc, i) =>
        acc :+ i
    }
    bh.consume(ys)
  }

  @Benchmark
  def expand_foldAppendDouble(bh: Blackhole): Unit = {
    val ys = (1L to size).foldLeft(empty) {
      case (acc, i) =>
        bh.consume(acc :+ 42L)
        acc :+ i
    }
    bh.consume(ys)
  }

  @Benchmark
  def expand_foldAppendDoubleForeach(bh: Blackhole): Unit = {
    val ys = (1L to size).foldLeft(empty) {
      case (acc, i) =>
        bh.consume(acc :+ 42L)
        acc :+ i
    }
    ys.foreach(bh.consume)
  }

  @Benchmark
  def expand_concat(bh: Blackhole): Unit = bh.consume(xs ++ zs)

  @Benchmark
  def extract_palindrome(bh: Blackhole): Unit = {
    @tailrec def isPalindrome[A](xs: Seq[A]): Boolean = {
      xs match {
        case first +: middle :+ last => first == last && isPalindrome(middle)
        case _ => true
      }
    }
    bh.consume(isPalindrome(ys))
  }

  @Benchmark
  @OperationsPerInvocation(1000)
  def expand_prepend(bh: Blackhole): Unit = {
    var ys = xs
    var i = 0L
    while (i < 1000) {
      ys = i #:: ys
      i += 1
    }
    bh.consume(ys)
  }

  @Benchmark
  @OperationsPerInvocation(1000)
  def expand_prependTail(bh: Blackhole): Unit = {
    var ys = xs
    var i = 0L
    while (i < 1000) {
      ys = i #:: ys
      i += 1
      ys = ys.tail
    }
    bh.consume(ys)
  }

  @Benchmark
  @OperationsPerInvocation(1000)
  def expand_append(bh: Blackhole): Unit = {
    var ys = xs
    var i = 0L
    while (i < 1000) {
      ys = ys :+ i
      i += 1
    }
    bh.consume(ys)
  }

  @Benchmark
  @OperationsPerInvocation(1000)
  def expand_appendInit(bh: Blackhole): Unit = {
    var ys = xs
    var i = 0L
    while (i < 1000) {
      ys = ys :+ i
      i += 1
      ys = ys.init
    }
    bh.consume(ys)
  }

  @Benchmark
  @OperationsPerInvocation(1000)
  def expand_prependAppend(bh: Blackhole): Unit = {
    var ys = xs
    var i = 0L
    while (i < 1000) {
      if ((i & 1) == 1) ys = ys :+ i
      else ys = i #:: ys
      i += 1
    }
    bh.consume(ys)
  }

  @Benchmark
  @OperationsPerInvocation(1000)
  def expand_prependAll(bh: Blackhole): Unit = {
    var ys = xs
    var i = 0L
    while (i < 1000) {
      ys = zs ++: ys
      i += 1
    }
    bh.consume(ys)
  }

  @Benchmark
  @OperationsPerInvocation(1000)
  def expand_appendAll(bh: Blackhole): Unit = {
    var ys = xs
    var i = 0L
    while (i < 1000) {
      ys = ys :++ zs
      i += 1
    }
    bh.consume(ys)
  }

  @Benchmark
  @OperationsPerInvocation(1000)
  def expand_prependAllAppendAll(bh: Blackhole): Unit = {
    var ys = xs
    var i = 0L
    while (i < 1000) {
      if ((i & 1) == 1) ys = ys :++ zs
      else ys = zs ++: ys
      i += 1
    }
    bh.consume(ys)
  }

  @Benchmark
  def expand_padTo(bh: Blackhole): Unit = {
    bh.consume(xs.padTo(size * 2, 42L))
  }

  @Benchmark
  def traverse_indexed(bh: Blackhole): Unit = {
    var n = 0L
    var i = 0
    val sz = xs.size
    while (i < sz) {
      n += xs(i)
      bh.consume(n)
      i += 1
    }
    bh.consume(n)
  }

  @Benchmark
  def traverse_foreach(bh: Blackhole): Unit = {
    var n = 0L
    xs.foreach { x =>
      n += x
      bh.consume(n)
    }
    bh.consume(n)
  }

  @Benchmark
  def traverse_headTail(bh: Blackhole): Unit = {
    var n = 0L
    var ys = xs
    while (ys.nonEmpty) {
      n += ys.head
      bh.consume(n)
      ys = ys.tail
    }
    bh.consume(n)
  }

  @Benchmark
  def traverse_initLast(bh: Blackhole): Unit = {
    var n = 0L
    var ys = xs
    while (ys.nonEmpty) {
      n += ys.last
      bh.consume(n)
      ys = ys.init
    }
    bh.consume(n)
  }

  @Benchmark
  def traverse_iterator(bh: Blackhole): Unit = {
    var n = 0L
    val it = xs.iterator
    while (it.hasNext) {
      n += it.next()
      bh.consume(n)
    }
    bh.consume(n)
  }

  @Benchmark
  def traverse_foldLeft(bh: Blackhole): Unit = bh.consume(xs.foldLeft(0L) {
    case (acc, n) =>
      bh.consume(n)
      acc + n
  })

  @Benchmark
  def traverse_foldRight(bh: Blackhole): Unit = bh.consume(xs.foldRight(0L) {
    case (n, acc) =>
      bh.consume(n)
      acc - n
  })

  @Benchmark
  @OperationsPerInvocation(1000)
  def access_last(bh: Blackhole): Unit = {
    var i = 0
    while (i < 1000) {
      bh.consume(xs.last)
      i += 1
    }
  }

  @Benchmark
  @OperationsPerInvocation(1000)
  def access_random(bh: Blackhole): Unit = {
    var i = 0
    while (i < 1000) {
      bh.consume(xs(randomIndices(i)))
      i += 1
    }
  }

  @Benchmark
  def access_tail(bh: Blackhole): Unit = bh.consume(xs.tail)

  @Benchmark
  def access_init(bh: Blackhole): Unit = bh.consume(xs.init)

  @Benchmark
  @OperationsPerInvocation(100)
  def access_slice(bh: Blackhole): Unit = {
    var i = 0
    while (i < 100) {
      bh.consume(xs.slice(i * size / 200, size - i * size / 200))
      i += 1
    }
  }

  @Benchmark
  @OperationsPerInvocation(100)
  def access_indexOfSlice(bh: Blackhole): Unit = {
    var i = 0
    var n = 0L
    while (i < 100) {
      n += xs.indexOfSlice(xs.drop(i))
      i += 1
    }
    bh.consume(n)
  }

  @Benchmark
  @OperationsPerInvocation(1000)
  def transform_updateLast(bh: Blackhole): Unit = {
    var i = 0L
    while (i < 1000) {
      bh.consume(xs.updated(size - 1, i))
      i += 1
    }
  }

  @Benchmark
  @OperationsPerInvocation(1000)
  def transform_updateForeach(bh: Blackhole): Unit = {
    var ys = xs
    var i = 0
    while (i < 1000) {
      ys = ys.updated(randomIndices(i), i.toLong)
      bh.consume(ys)
      i += 1
    }
    ys.foreach(bh.consume)
  }

  @Benchmark
  @OperationsPerInvocation(1000)
  def transform_updateRandom(bh: Blackhole): Unit = {
    var ys = xs
    var i = 0
    while (i < 1000) {
      ys = ys.updated(randomIndices(i), i.toLong)
      bh.consume(ys)
      i += 1
    }
    bh.consume(ys)
  }

  @Benchmark
  @OperationsPerInvocation(100)
  def transform_patch(bh: Blackhole): Unit = {
    var i = 0
    while (i < 100) {
      val from = randomIndices(i)
      val replaced = randomIndices(if (i > 0) i - 1 else math.min(i + 1, size - 1))
      val length = randomIndices(if (i > 1) i - 2 else math.min(i + 2, size - 1))
      bh.consume(xs.patch(from, xs.take(length), replaced))
      i += 1
    }
  }

  @Benchmark
  def transform_distinct(bh: Blackhole): Unit = bh.consume(xs.distinct)

  @Benchmark
  def transform_distinctBy(bh: Blackhole): Unit = bh.consume(xs.distinctBy(_ % 2L))

  @Benchmark
  def transform_map(bh: Blackhole): Unit = bh.consume(xs.map((n: Long) => n + 1L))

  @Benchmark
  def transform_collect(bh: Blackhole): Unit = bh.consume(xs.collect {
    case n if n % 5L == 0L => n * 5L
    case n if n % 3L == 0L => n * 3L
    case n if n == size - 1 => n * n
  })

  @Benchmark
  def transform_flatMap(bh: Blackhole): Unit = bh.consume(xs.flatMap {
    case n if n % 5L == 0L => List.range(1L, n / 5, 5)
    case n if n % 3L == 0L => List(n, -n)
    case n if n == size - 1 => List.range(1L, n)
    case _ => Nil
  })

  @Benchmark
  def transform_filter(bh: Blackhole): Unit = bh.consume(xs.filter(_ % 5L == 0L))

  @Benchmark
  @OperationsPerInvocation(100)
  def transform_span(bh: Blackhole): Unit = {
    var i = 0
    while (i < 100) {
      val (xs1, xs2) = xs.span(x => x < randomIndices(i).toLong)
      bh.consume(xs1)
      bh.consume(xs2)
      i += 1
    }
  }

  @Benchmark
  def transform_zip(bh: Blackhole): Unit = bh.consume(xs.zip(xs))

  @Benchmark
  def transform_zipMapTupled(bh: Blackhole): Unit = {
    val f = (a: Long, b: Long) => (a, b)
    bh.consume(xs.zip(xs).map(f.tupled))
  }

  @Benchmark
  def transform_zipWithIndex(bh: Blackhole): Unit = bh.consume(xs.zipWithIndex)

  @Benchmark
  def transform_lazyZip(bh: Blackhole): Unit = bh.consume(xs.lazyZip(xs).map((_, _)))

  @Benchmark
  def transform_unzip(bh: Blackhole): Unit = bh.consume(zipped.unzip)

  @Benchmark
  def transform_reverse(bh: Blackhole): Unit = bh.consume(xs.reverse)

  @Benchmark
  def transform_groupBy(bh: Blackhole): Unit = {
    bh.consume(xs.groupBy(_ % 5L))
  }

  @Benchmark
  def traverse_equals(bh: Blackhole): Unit = bh.consume(xs == ys)

  @Benchmark
  def access_find(bh: Blackhole): Unit =
    bh.consume(xs.find(x => x > size / 2))
}