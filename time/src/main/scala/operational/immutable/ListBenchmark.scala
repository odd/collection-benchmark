package operational.immutable

import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

@BenchmarkMode(scala.Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@Fork(1)
@Warmup(iterations = 8)
@Measurement(iterations = 8)
@State(Scope.Benchmark)
class ListBenchmark {
  @Param(scala.Array("0", "1", "2", "3", "4", "7", "8", "15", "16", "17", "39", "282", "4096", "131070", "7312102"))
  var size: Int = _

  var xs: List[Long] = _
  var zs: List[Long] = _
  var zipped: List[(Long, Long)] = _
  var randomIndices: scala.Array[Int] = _
  def fresh(n: Int) = List((1 to n).map(_.toLong): _*)

  @Setup(Level.Trial)
  def initTrial(): Unit = {
    xs = fresh(size)
    zs = fresh((size / 1000) max 2).map(-_)
    zipped = xs.map(x => (x, x))
    if (size > 0) {
      randomIndices = scala.Array.fill(1000)(scala.util.Random.nextInt(size))
    }
  }

  @Benchmark
  def create(bh: Blackhole): Unit = bh.consume(fresh(size))

  @Benchmark
  @OperationsPerInvocation(1000)
  def expand_prepend(bh: Blackhole): Unit = {
    var ys = xs
    var i = 0L
    while (i < 1000) {
      ys = i :: ys
      i += 1
    }
    bh.consume(ys)
  }

//  @Benchmark
//  @OperationsPerInvocation(1000)
//  def expand_prependTail(bh: Blackhole): Unit = {
//    var ys = xs
//    var i = 0L
//    while (i < 1000) {
//      ys = i :: ys
//      i += 1
//      ys = ys.tail
//    }
//    bh.consume(ys)
//  }

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

//  @Benchmark
//  @OperationsPerInvocation(1000)
//  def expand_appendInit(bh: Blackhole): Unit = {
//    var ys = xs
//    var i = 0L
//    while (i < 1000) {
//      ys = ys :+ i
//      i += 1
//      ys = ys.init
//    }
//    bh.consume(ys)
//  }

//  @Benchmark
//  @OperationsPerInvocation(1000)
//  def expand_prependAppend(bh: Blackhole): Unit = {
//    var ys = xs
//    var i = 0L
//    while (i < 1000) {
//      if ((i & 1) == 1) ys = ys :+ i
//      else ys = i :: ys
//      i += 1
//    }
//    bh.consume(ys)
//  }

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

//  @Benchmark
//  @OperationsPerInvocation(1000)
//  def expand_prependAllAppendAll(bh: Blackhole): Unit = {
//    var ys = xs
//    var i = 0L
//    while (i < 1000) {
//      if ((i & 1) == 1) ys = ys :++ zs
//      else ys = zs ++: ys
//      i += 1
//    }
//    bh.consume(ys)
//  }

  @Benchmark
  def expand_padTo(bh: Blackhole): Unit = bh.consume(xs.padTo(size * 2, 42))

  @Benchmark
  def traverse_foreach(bh: Blackhole): Unit = xs.foreach(x => bh.consume(x))

//  @Benchmark
//  def traverse_headTail(bh: Blackhole): Unit = {
//    var ys = xs
//    while (ys.nonEmpty) {
//      bh.consume(ys.head)
//      ys = ys.tail
//    }
//  }
//
//  @Benchmark
//  def traverse_initLast(bh: Blackhole): Unit = {
//    var ys = xs
//    while (ys.nonEmpty) {
//      bh.consume(ys.last)
//      ys = ys.init
//    }
//  }

  @Benchmark
  def traverse_iterator(bh: Blackhole): Unit = {
    val it = xs.iterator
    while (it.hasNext) {
      bh.consume(it.next())
    }
  }

  @Benchmark
  def traverse_foldLeft(bh: Blackhole): Unit = bh.consume(xs.foldLeft(0) {
    case (acc, n) =>
      bh.consume(n)
      acc + 1
  })

  @Benchmark
  def traverse_foldRight(bh: Blackhole): Unit = bh.consume(xs.foldRight(0) {
    case (n, acc) =>
      bh.consume(n)
      acc - 1
  })

  @Benchmark
  @OperationsPerInvocation(1000)
  def access_last(bh: Blackhole): Unit = {
    var i = 0
    while (i < 1000) {
      bh.consume(xs(size - 1))
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
      bh.consume(xs.slice(size - size / (i + 1), size))
      i += 1
    }
  }

  @Benchmark
  @OperationsPerInvocation(1000)
  def transform_updateLast(bh: Blackhole): Unit = {
    var i = 0
    while (i < 1000) {
      bh.consume(xs.updated(size - 1, i))
      i += 1
    }
  }

  @Benchmark
  @OperationsPerInvocation(1000)
  def transform_updateRandom(bh: Blackhole): Unit = {
    var i = 0
    while (i < 1000) {
      bh.consume(xs.updated(randomIndices(i), i))
      i += 1
    }
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

//  @Benchmark
//  def transform_distinctBy(bh: Blackhole): Unit = bh.consume(xs.distinctBy(_ % 2))

  @Benchmark
  def transform_map(bh: Blackhole): Unit = bh.consume(xs.map(x => x + 1))

  @Benchmark
  @OperationsPerInvocation(100)
  def transform_span(bh: Blackhole): Unit = {
    var i = 0
    while (i < 100) {
      val (xs1, xs2) = xs.span(x => x < randomIndices(i))
      bh.consume(xs1)
      bh.consume(xs2)
      i += 1
    }
  }

  @Benchmark
  def transform_zip(bh: Blackhole): Unit = bh.consume(xs.zip(xs))

//  @Benchmark
//  def transform_zipMapTupled(bh: Blackhole): Unit = {
//    val f = (a: Long, b: Long) => (a, b)
//    bh.consume(xs.zip(xs).map(f.tupled))
//  }
//
//  @Benchmark
//  def transform_zipWithIndex(bh: Blackhole): Unit = bh.consume(xs.zipWithIndex)
//
//  @Benchmark
//  def transform_lazyZip(bh: Blackhole): Unit = bh.consume(xs.lazyZip(xs).map((_, _)))

  @Benchmark
  def transform_unzip(bh: Blackhole): Unit = bh.consume(zipped.unzip)

  @Benchmark
  def transform_reverse(bh: Blackhole): Unit = bh.consume(xs.reverse)

  @Benchmark
  def transform_groupBy(bh: Blackhole): Unit = {
    val result = xs.groupBy(_ % 5)
    bh.consume(result)
  }

  @Benchmark
  def access_find(bh: Blackhole): Unit =
    xs.find(x => x > size / 2)
}