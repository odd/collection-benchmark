package collection.benchmark
package immutable

import scala.collection.immutable.{HashSet, ListSet, NumericRange, TreeSet}
import scala.collection.mutable
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

class SetBenchmark extends AbstractIterableBenchmark {
  @Param(scala.Array("HashSet", "ListSet", "TreeSet"))
  var impl: String = _
  var xs: Set[Long] = _
  var ys: Set[Long] = _
  var zs: Set[Long] = _
  def create(n: Int = 0): Set[Long] = {
    impl match {
      case "HashSet" => HashSet(1L until n: _*)
      case "ListSet" => ListSet(1L until n: _*)
      case "TreeSet" => TreeSet(1L until n: _*)
    }
  }
  override def zero = 0L
  override def successor = _ + 1L
  override def mapper = -_
  override def combiner = _ + _
  override def spanner = _ < size / 2L
  override def grouper = _ % 5L
  override def finder = n => n > size / 2L

  @Setup(Level.Trial)
  def initTrial(): Unit = {
    xs = create(size)
    ys = create(size / 2) ++ create(size / 2).map(mapper)
    zs = create((size / 1000).max(2))
    if (size > 0) {
      randomIndices = scala.Array.fill(1000)(random.nextInt(size))
    }
  }

  @Benchmark
  @OperationsPerInvocation(1000)
  def expand_incl(bh: Blackhole): Unit = {
    except("expand_incl")
    var ys = xs
    var i = 0L
    while (i < 1000) {
      ys += -i
      i += 1
    }
    bh.consume(ys)
  }

  @Benchmark
  @OperationsPerInvocation(1000)
  def access_contains(bh: Blackhole): Unit = {
    except("access_contains")
    var i = 0
    while (i < 1000) {
      bh.consume(xs.contains(i))
      i += 1
    }
  }

  @Benchmark
  def transform_lazyZip(bh: Blackhole): Unit = {
    except("transform_lazyZip")
    bh.consume(xs.lazyZip(xs).map((_, _)))
  }

  @Benchmark
  def traverse_subsetOf(bh: Blackhole): Unit = {
    except("traverse_subsetOf")
    bh.consume(ys.subsetOf(xs))
  }
}