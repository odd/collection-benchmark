package collection.benchmark
package immutable

import scala.collection.immutable._
import org.openjdk.jmh.annotations._

class MapBenchmark extends AbstractMapBenchmark {
  @Param(scala.Array("HashMap", "LongMap", "TreeMap"))
  var impl: String = _

  def create(n: Int = 0): Map[Long, Long] = {
    impl match {
      case "HashMap" => HashMap((0L until size).map(n => (n, -n)): _*)
      case "LongMap" => LongMap((0L until size).map(n => (n, -n)): _*)
      case "TreeMap" => TreeMap((0L until size).map(n => (n, -n)): _*)
    }
  }
}