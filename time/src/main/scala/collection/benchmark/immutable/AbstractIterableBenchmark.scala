package collection.benchmark
package immutable

import scala.collection.immutable._
import scala.collection.mutable
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

abstract class AbstractIterableBenchmark extends AbstractBenchmark {
  //@Param(scala.Array("0", "1", "2", "3", "4", "7", "8", "15", "16", "17", "39", "282", "4096", "131070", "7312102"))
  @Param(scala.Array("7", "65", "1024")) //7", "65", "4096", "128000"))
  var size: Int = _
  def xs: Iterable[Long]
  def ys: Iterable[Long]
  def zs: Iterable[Long]
  val random = new scala.util.Random(19740115L)
  var randomIndices: scala.Array[Int] = _

  def create(n: Int = 0): Iterable[Long]
  def zero: Long
  def successor: Long => Long
  def mapper: Long => Long
  def grouper: Long => Long
  def combiner: (Long, Long) => Long
  def finder: Long => Boolean
  def spanner: Long => Boolean

  @Benchmark
  def expand_concat(bh: Blackhole): Unit = {
    except("expand_concat")
    bh.consume(xs ++ zs)
  }

  @Benchmark
  def traverse_iterator(bh: Blackhole): Unit = {
    except("traverse_iterator")
    val it = xs.iterator
    while (it.hasNext) {
      bh.consume(it.next())
    }
  }

  @Benchmark
  def traverse_foreach(bh: Blackhole): Unit = {
    except("traverse_foreach")
    xs.foreach(bh.consume)
  }

  @Benchmark
  def traverse_headTail(bh: Blackhole): Unit = {
    except("traverse_headTail")
    var ys = xs
    while (ys.nonEmpty) {
      bh.consume(ys.head)
      ys = ys.tail
    }
  }

  @Benchmark
  def traverse_initLast(bh: Blackhole): Unit = {
    except("traverse_initLast")
    var ys = xs
    while (ys.nonEmpty) {
      bh.consume(ys.last)
      ys = ys.init
    }
  }

  @Benchmark
  def traverse_foldLeft(bh: Blackhole): Unit = {
    except("traverse_foldLeft")
    bh.consume(xs.foldLeft(zero) {
      case (acc, t) =>
        bh.consume(acc)
        combiner(acc, t)
    })
  }

  @Benchmark
  def traverse_foldRight(bh: Blackhole): Unit = {
    except("traverse_foldRight")
    bh.consume(xs.foldRight(zero) {
      case (t, acc) =>
        bh.consume(acc)
        combiner(acc, t)
      })
  }

  @Benchmark
  @OperationsPerInvocation(1000)
  def access_last(bh: Blackhole): Unit = {
    except("access_last")
    var i = 0
    while (i < 1000) {
      bh.consume(xs.last)
      i += 1
    }
    bh.consume(i)
  }

  @Benchmark
  @OperationsPerInvocation(100)
  def access_tail(bh: Blackhole): Unit = {
    except("access_tail")
    var i = 0
    while (i < 1000) {
      bh.consume(xs.tail)
      i += 1
    }
    bh.consume(i)
  }

  @Benchmark
  @OperationsPerInvocation(100)
  def access_init(bh: Blackhole): Unit = {
    except("access_init")
    var i = 0
    while (i < 1000) {
      bh.consume(xs.init)
      i += 1
    }
    bh.consume(i)
  }

  @Benchmark
  @OperationsPerInvocation(100)
  def access_slice(bh: Blackhole): Unit = {
    except("access_slice")
    var i = 0
    while (i < 100) {
      bh.consume(xs.slice(i * size / 200, size - i * size / 200))
      i += 1
    }
  }

  @Benchmark
  def transform_map(bh: Blackhole): Unit = {
    except("transform_map")
    bh.consume(xs.map(mapper))
  }

  @Benchmark
  def transform_map2(bh: Blackhole): Unit = {
    except("transform_map2")
    xs match {
      case as: ArraySeq.ofLong => bh.consume(as.map(mapper))
      case _ => bh.consume(xs.map(mapper))
    }
  }

  @Benchmark
  def transform_map3(bh: Blackhole): Unit = {
    except("transform_map3")
    xs match {
      case xs: ArraySeq[Long] => bh.consume(xs.map(mapper))
      case _ => bh.consume(xs.map(mapper))
    }
  }

  @Benchmark
  def transform_collect(bh: Blackhole): Unit = {
    except("transform_collect")
    bh.consume(xs.collect {
      case t if t % 5L == 0L =>
        val t2 = mapper(t)
        bh.consume(t2)
        t2
      case t if t % 3L == 0L =>
        val t2 = mapper(t)
        bh.consume(t2)
        t2
      case t if t == size - 1 =>
        val t2 = mapper(t)
        bh.consume(t2)
        t2
    })
  }

  @Benchmark
  def transform_collect2(bh: Blackhole): Unit = {
    except("transform_collect2")
    xs match {
      case xs: ArraySeq[Long] =>
        bh.consume(xs.collect {
          case t if t % 5L == 0L =>
            val t2 = mapper(t)
            bh.consume(t2)
            t2
          case t if t % 3L == 0L =>
            val t2 = mapper(t)
            bh.consume(t2)
            t2
          case t if t == size - 1 =>
            val t2 = mapper(t)
            bh.consume(t2)
            t2
        })
      case _ =>
        bh.consume(xs.collect {
          case t if t % 5L == 0L =>
            val t2 = mapper(t)
            bh.consume(t2)
            t2
          case t if t % 3L == 0L =>
            val t2 = mapper(t)
            bh.consume(t2)
            t2
          case t if t == size - 1 =>
            val t2 = mapper(t)
            bh.consume(t2)
            t2
        })
    }
  }

  @Benchmark
  def transform_collect3(bh: Blackhole): Unit = {
    except("transform_collect3")
    xs match {
      case xs: ArraySeq.ofLong =>
        bh.consume(xs.collect {
          case t if t % 5L == 0L =>
            val t2 = mapper(t)
            bh.consume(t2)
            t2
          case t if t % 3L == 0L =>
            val t2 = mapper(t)
            bh.consume(t2)
            t2
          case t if t == size - 1 =>
            val t2 = mapper(t)
            bh.consume(t2)
            t2
        })
      case _ =>
        bh.consume(xs.collect {
          case t if t % 5L == 0L =>
            val t2 = mapper(t)
            bh.consume(t2)
            t2
          case t if t % 3L == 0L =>
            val t2 = mapper(t)
            bh.consume(t2)
            t2
          case t if t == size - 1 =>
            val t2 = mapper(t)
            bh.consume(t2)
            t2
        })
    }
  }

  @Benchmark
  def transform_flatMap(bh: Blackhole): Unit = {
    except("transform_flatMap")
    bh.consume(xs.flatMap {
      case t if t % 5L == 0L =>
        bh.consume(t)
        LazyList.range(1L, t / 5, 5)
      case t if t % 3L == 0L =>
        bh.consume(t)
        LazyList(t, -t)
      case t if t == size - 1 =>
        bh.consume(t)
        LazyList.range(1L, t)
      case _ =>
        LazyList()
    })
  }

  @Benchmark
  def transform_flatMap2(bh: Blackhole): Unit = {
    except("transform_flatMap2")
    xs match {
      case xs: ArraySeq[Long] =>
        bh.consume(xs.flatMap {
          case t if t % 5L == 0L =>
            bh.consume(t)
            LazyList.range(1L, t / 5, 5)
          case t if t % 3L == 0L =>
            bh.consume(t)
            LazyList(t, -t)
          case t if t == size - 1 =>
            bh.consume(t)
            LazyList.range(1L, t)
          case _ =>
            LazyList()
        })
      case _ =>
        bh.consume(xs.flatMap {
          case t if t % 5L == 0L =>
            bh.consume(t)
            LazyList.range(1L, t / 5, 5)
          case t if t % 3L == 0L =>
            bh.consume(t)
            LazyList(t, -t)
          case t if t == size - 1 =>
            bh.consume(t)
            LazyList.range(1L, t)
          case _ =>
            LazyList()
        })
    }
  }

  @Benchmark
  def transform_flatMap3(bh: Blackhole): Unit = {
    except("transform_flatMap3")
    xs match {
      case xs: ArraySeq.ofLong =>
        bh.consume(xs.flatMap {
          case t if t % 5L == 0L =>
            bh.consume(t)
            LazyList.range(1L, t / 5, 5)
          case t if t % 3L == 0L =>
            bh.consume(t)
            LazyList(t, -t)
          case t if t == size - 1 =>
            bh.consume(t)
            LazyList.range(1L, t)
          case _ =>
            LazyList()
        })
      case _ =>
        bh.consume(xs.flatMap {
          case t if t % 5L == 0L =>
            bh.consume(t)
            LazyList.range(1L, t / 5, 5)
          case t if t % 3L == 0L =>
            bh.consume(t)
            LazyList(t, -t)
          case t if t == size - 1 =>
            bh.consume(t)
            LazyList.range(1L, t)
          case _ =>
            LazyList()
        })
    }
  }

  @Benchmark
  def transform_filter(bh: Blackhole): Unit = {
    except("transform_filter")
    bh.consume(xs.filter(_ % 5L == 0L))
  }

  @Benchmark
  @OperationsPerInvocation(100)
  def transform_span(bh: Blackhole): Unit = {
    except("transform_span")
    var i = 0
    while (i < 100) {
      val (xs1, xs2) = xs.span(spanner)
      bh.consume(xs1)
      bh.consume(xs2)
      i += 1
    }
  }

  @Benchmark
  def transform_zip(bh: Blackhole): Unit = {
    except("transform_zip")
    bh.consume(xs.zip(xs))
  }

  @Benchmark
  def transform_zipMapTupled(bh: Blackhole): Unit = {
    except("transform_zipMapTupled")
    val f: (Long, Long) => Long = { case (a: Long, b: Long) => a + b }
    bh.consume(xs.zip(xs).map(f.tupled))
  }

  @Benchmark
  def transform_zipWithIndex(bh: Blackhole): Unit = {
    except("transform_zipWithIndex")
    bh.consume(xs.zipWithIndex)
  }

  @Benchmark
  def transform_groupBy(bh: Blackhole): Unit = {
    except("transform_groupBy")
    bh.consume(xs.groupBy(grouper))
  }

  @Benchmark
  def traverse_equals(bh: Blackhole): Unit = {
    except("traverse_equals")
    bh.consume(xs == ys)
  }

  @Benchmark
  def access_find(bh: Blackhole): Unit = {
    except("access_find")
    bh.consume(xs.find(finder))
  }
}