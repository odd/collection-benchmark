package ops.immutable

import java.util.concurrent.TimeUnit
import scala.collection.immutable.{LinkedHashMap, NumericRange}
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

@BenchmarkMode(scala.Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@Fork(1)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
@State(Scope.Benchmark)
class LinkedHashMapBenchmark {
  //@Param(scala.Array("0", "1", "2", "3", "4", "7", "8", "15", "16", "17", "39", "282", "4096", "131070", "7312102"))
  @Param(scala.Array("1", "7", "4096", "128000"))
  var size: Int = _

  val random = new scala.util.Random(19740115L)
  val empty: LinkedHashMap[Long, Long] = LinkedHashMap.empty[Long, Long]
  var xs: LinkedHashMap[Long, Long] = _
  var zs: LinkedHashMap[Long, Long] = _
  var zipped: LinkedHashMap[Long, (Long, Long)] = _
  var randomIndices: scala.Array[Int] = _
  def fresh(n: Int) = LinkedHashMap((1 to n).map(x => (x.toLong, x.toLong)): _*)
  def freshBuilder() = LinkedHashMap.newBuilder[Long, Long]

  @Setup(Level.Trial)
  def initTrial(): Unit = {
    xs = fresh(size)
    zs = fresh((size / 1000).max(2)).map { case (k, v) => (k, -v) }
    zipped = xs.map { case (k, v)                      => (k, (v, v)) }
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
      builder += i -> i
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
      builder ++= NumericRange(i - 10, i, 1).map(n => n -> n)
      i += 1
    }
    bh.consume(builder.result())
  }

  @Benchmark
  @OperationsPerInvocation(1000)
  def expand_updated(bh: Blackhole): Unit = {
    var ys = xs
    var i = 0L
    while (i < 1000) {
      ys += (i -> -i)
      i += 1
    }
    bh.consume(ys)
  }

  @Benchmark
  def expand_concat(bh: Blackhole): Unit = bh.consume(xs ++ zs)

  @Benchmark
  def traverse_foreach(bh: Blackhole): Unit = {
    var n = 0L
    xs.foreach { x =>
      n += x._2
      bh.consume(n)
    }
    bh.consume(n)
  }

  @Benchmark
  def traverse_headTail(bh: Blackhole): Unit = {
    var n = 0L
    var ys = xs
    while (ys.nonEmpty) {
      n += ys.head._2
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
      n += ys.last._2
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
      n += it.next()._2
      bh.consume(n)
    }
    bh.consume(n)
  }

  @Benchmark
  def traverse_foldLeft(bh: Blackhole): Unit =
    bh.consume(xs.foldLeft(0L) {
      case (acc, n) =>
        bh.consume(n)
        acc + n._2
    })

  @Benchmark
  def traverse_foldRight(bh: Blackhole): Unit =
    bh.consume(xs.foldRight(0L) {
      case (n, acc) =>
        bh.consume(n)
        acc - n._2
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
      bh.consume(xs(randomIndices(i) + 1))
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
  @OperationsPerInvocation(1000)
  def access_contains(bh: Blackhole): Unit = {
    var i = 0
    while (i < 1000) {
      bh.consume(xs.contains(i))
      i += 1
    }
  }

  @Benchmark
  def transform_map(bh: Blackhole): Unit =
    bh.consume(xs.map {
      case (n: Long, m: Long) => (n + 1L) -> (m - 1L)
    })

  @Benchmark
  def transform_collect(bh: Blackhole): Unit =
    bh.consume(xs.collect {
      case (n: Long, m: Long) if n % 5L == 0L  => (n * 5L, m)
      case (n: Long, m: Long) if n % 3L == 0L  => (n * 3L, m)
      case (n: Long, m: Long) if n == size - 1 => (n * n, m)
    })

  @Benchmark
  def transform_flatMap(bh: Blackhole): Unit =
    bh.consume(xs.flatMap {
      case (n: Long, m: Long) if n % 5L == 0L  => List.range(1L, n / 5, 5)
      case (n: Long, m: Long) if n % 3L == 0L  => List(n, -n)
      case (n: Long, m: Long) if n == size - 1 => List.range(1L, n)
      case _                                   => Nil
    })

  @Benchmark
  def transform_filter(bh: Blackhole): Unit =
    bh.consume(xs.filter(_._2 % 5L == 0L))

  @Benchmark
  @OperationsPerInvocation(100)
  def transform_span(bh: Blackhole): Unit = {
    var i = 0
    while (i < 100) {
      val (xs1, xs2) = xs.span(x => x._2 < randomIndices(i).toLong)
      bh.consume(xs1)
      bh.consume(xs2)
      i += 1
    }
  }

  @Benchmark
  def transform_zip(bh: Blackhole): Unit = bh.consume(xs.zip(xs))

  @Benchmark
  def transform_zipWithIndex(bh: Blackhole): Unit = bh.consume(xs.zipWithIndex)

  @Benchmark
  def transform_lazyZip(bh: Blackhole): Unit =
    bh.consume(xs.lazyZip(xs).map((_, _)))

  @Benchmark
  def transform_unzip(bh: Blackhole): Unit = bh.consume(zipped.unzip)

  @Benchmark
  def transform_groupBy(bh: Blackhole): Unit = {
    bh.consume(xs.groupBy(_._2 % 5L))
  }

  @Benchmark
  def transform_remove(bh: Blackhole): Unit = {
    bh.consume(xs.removeAll(randomIndices.map(_ + 1L)))
  }

  @Benchmark
  def transform_removeIterator(bh: Blackhole): Unit = {
    var n = 0L
    val it = xs.removeAll(randomIndices.map(_ + 1L)).iterator
    while (it.hasNext) {
      n += it.next()._2
      bh.consume(n)
    }
    bh.consume(n)
  }

  @Benchmark
  def transform_removeConsecutive(bh: Blackhole): Unit = {
    bh.consume(xs.removeAll(Seq.range[Long](size / 50, size - (size / 50))))
  }

  @Benchmark
  def transform_removeConsecutiveIterator(bh: Blackhole): Unit = {
    var n = 0L
    val it = xs.removeAll(Seq.range[Long](size / 50, size - (size / 50))).iterator
    while (it.hasNext) {
      n += it.next()._2
      bh.consume(n)
    }
    bh.consume(n)
  }

  @Benchmark
  def traverse_equals(bh: Blackhole): Unit = bh.consume(xs == zs)

  @Benchmark
  def access_find(bh: Blackhole): Unit =
    bh.consume(xs.find(x => x._2 > size / 2))
}
