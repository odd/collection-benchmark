package collection.benchmark

import scala.collection.immutable.ArraySeq
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

class StdDevBenchmark extends AbstractBenchmark {
  //@Param(scala.Array("0", "1", "2", "3", "4", "7", "8", "15", "16", "17", "39", "282", "4096", "131070", "7312102"))
  @Param(scala.Array("7", "65", "1024"/*, "4096"*//*, "128000"*/))
  var size: Int = _
  @Param(scala.Array("Baseline", "Array", "ArraySeq", "List", "Vector"))
  var impl: String = _
  var xs: scala.collection.Seq[Long] = _

  def create(size: Int = 0): Seq[Long] = {
    impl match {
      case "Baseline" => Array[Long](0L until size: _*)
      case "Array" => Array[Long](0L until size: _*)
      case "ArraySeq" => ArraySeq(0L until size: _*)
      case "List" => List(0L until size: _*)
      case "Vector" => Vector(0L until size: _*)
    }
  }

  @Setup(Level.Trial)
  def initTrial(): Unit = {
    xs = create(size)
  }

  @Benchmark
  def stddev(bh: Blackhole): Unit = {
    val mean = xs.sum / xs.size
    val squared =
      xs.map(x => (x - mean) * (x - mean)).sum
    bh.consume(squared)
  }
}
