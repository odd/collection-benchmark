package collection.benchmark
package immutable

import scala.collection.immutable._
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

class SeqMapBenchmark extends AbstractMapBenchmark {
  //@Param(scala.Array("TreeSeqMapO", "TreeSeqMapX", "VectorMap"))
  @Param(scala.Array("TreeSeqMap", "VectorMap"))
  var impl: String = _

  def create(n: Int = 0): Map[Long, Long] = {
    impl match {
      case "ListMap" => ListMap((0L until size).map(n => (n, -n)): _*)
      case "TreeSeqMap" => TreeSeqMap((0L until size).map(n => (n, -n)): _*)
      case "VectorMap" => VectorMap((0L until size).map(n => (n, -n)): _*)
    }
  }
  override def exceptions = {
    case "transform_removeConsecutive" => size > 4096 && (impl.contains("VectorMap") || impl.contains("ListMap"))
    case "transform_removeConsecutiveIterator" => size > 4096 && (impl.contains("VectorMap") || impl.contains("ListMap"))
  }
  /*
  @Benchmark
  def transform_removeConsecutive(bh: Blackhole): Unit = {
    except("transform_removeConsecutive")
    bh.consume(xs.removedAll(Seq.range[Long](size / 50, size - (size / 50))))
  }
  */

  @Benchmark
  def transform_removeConsecutiveIterator(bh: Blackhole): Unit = {
    except("transform_removeConsecutiveIterator")
    var n = 0L
    val it = xs.removedAll(Seq.range[Long](size / 50, size - (size / 50))).iterator
    while (it.hasNext) {
      n += it.next()._2
      bh.consume(n)
    }
    bh.consume(n)
  }
}
