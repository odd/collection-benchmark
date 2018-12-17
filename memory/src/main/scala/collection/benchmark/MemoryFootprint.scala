package collection.benchmark

import java.io.File
import java.nio.file.{Files, Paths}
import java.time.Instant
import scala.collection.immutable.{ArraySeq, HashSet, ListMap, NumericRange, OrderedMapX, TreeSet, VectorMap}
import scala.collection.mutable
import scala.compat.Platform

object MemoryFootprint extends App {
  val resultsDir = new File("memory/results")
  resultsDir.mkdirs()
  val reportPath = Paths.get("memory/results/memory-footprint-" + Instant.now().toString.replace(':', '-') + ".json")
  val sizes = scala.List(8, 64, 512, 4096, 32768) //, 262144) //, 2097152)
  val runtime = Runtime.getRuntime
  val obj: AnyRef = null
  var placeholder: Any = _

  def benchmark[A](gen: Int => A): scala.List[(Int, Long)] = (
    // We run 5 iterations and pick the last result only
    for (_ <- scala.Range(0, 5)) yield {
      for (size <- sizes) yield {
        placeholder = null
        Platform.collectGarbage()
        val memBefore = runtime.totalMemory() - runtime.freeMemory()
        placeholder = gen(size)
        Platform.collectGarbage()
        val memAfter = runtime.totalMemory() - runtime.freeMemory()
        size -> (memAfter - memBefore)
      }
    }
    ).last

  val memories =
    Map(
      "ListMap"               -> benchmark(n => ListMap.from((0 until n).map(n => n -> obj))),
      "OrderedMap"            -> benchmark(n => OrderedMapX.from((0 until n).map(n => n -> obj))),
      "VectorMap"             -> benchmark(n => VectorMap.from((0 until n).map(n => n -> obj))),
      "LinkedHashMap"         -> benchmark(n => mutable.LinkedHashMap.from((0 until n).map(n => n -> obj))),
      /*
      "Array"                 -> benchmark(Array.fill(_)(obj)),
      "ArraySeq"              -> benchmark(ArraySeq.fill(_)(obj)),
      "ArraySeq (primitive)"  -> benchmark(ArraySeq.fill(_)(123)),
      "Vector"                -> benchmark(Vector.fill(_)(obj)),
      "List"                  -> benchmark(List.fill(_)(obj)),
      "LazyList"              -> benchmark(LazyList.fill(_)(obj)),
      "Range"                 -> benchmark(Range(0, _)),
      "NumericRange"          -> benchmark(NumericRange(0, _, 1)),
      "HashSet"               -> benchmark(n => HashSet((1 to n).map(_.toString): _*)),
      "TreeSet"               -> benchmark(n => TreeSet((1 to n).map(_.toString): _*)),
      "ArrayBuffer"           -> benchmark(mutable.ArrayBuffer.fill(_)(obj)),
      "ListBuffer"            -> benchmark(mutable.ListBuffer.fill(_)(obj)),*/
    )

  // We use a format similar to the one used by JMH so that
  // our charts can be generated in the same way
  val report =
    memories.toList.flatMap { case (name, values) =>
      values.map { case (size, value) =>
        val metric = BenchmarkMetric(value, (value, value))
        BenchmarkResult(s"$name.memory-footprint", Map("size" -> size.toString), metric)
      }
    }

  Files.write(reportPath, format(report).getBytes("UTF-8"))

  def format(report: List[BenchmarkResult]): String = {
    report.map { result =>
      val paramsStr = (s"""      "impl" : "${result.benchmark}"""" :: result.params.map {
        case (name, value) => s"""      "$name" : "$value""""
      }.toList).mkString("{\n", ",\n", "\n    }")
      s"""
         |  {
         |    "jmhVersion" : "1.21",
         |    "benchmark" : "MemoryFootprint",
         |    "mode" : "avgt",
         |    "threads" : 1,
         |    "forks" : 1,
         |    "jvm" : "${System.getProperty("java.home")}",
         |    "jvmArgs" : [],
         |    "jdkVersion" : "${System.getProperty("java.version")}",
         |    "vmName" : "Java HotSpot(TM) 64-Bit Server VM",
         |    "vmVersion" : "25.181-b13",
         |    "warmupIterations" : 5,
         |    "warmupTime" : "200 ms",
         |    "warmupBatchSize" : 1,
         |    "measurementIterations" : 7,
         |    "measurementTime" : "500 ms",
         |    "measurementBatchSize" : 1,
         |    "params" : $paramsStr,
         |    "primaryMetric" : {
         |      "score" : ${result.primaryMetric.score},
         |      "scoreError" : 0,
         |      "scoreConfidence" : [
         |        ${result.primaryMetric.scoreConfidence._1},
         |        ${result.primaryMetric.scoreConfidence._2}
         |      ],
         |      "scorePercentiles" : {
         |        "100.0" : ${result.primaryMetric.score}
         |      },
         |      "scoreUnit" : "B/op",
         |      "rawData" : [
         |        [
         |          ${result.primaryMetric.score}
         |        ]
         |      ]
         |    },
         |    "secondaryMetrics" : {}
         |  }""".stripMargin
    }.mkString("[\n", ",\n", "\n]")
  }
}

case class BenchmarkMetric(score: Double, scoreConfidence: (Double, Double))
case class BenchmarkResult(benchmark: String, params: Map[String, String], primaryMetric: BenchmarkMetric)
