package footprint

import java.nio.file.{Files, Paths}
import scala.compat.Platform
import scala.collection.immutable._
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

object MemoryFootprint extends App {

  val reportPath = Paths.get(args(0))

  val sizes = scala.List(8, 64, 512, 4096, 32768, 262144, 2097152)

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
    scala.Predef.Map(
      "Array"                 -> benchmark(Array.fill(_)(obj)),
      "ArraySeq"              -> benchmark(ArraySeq.fill(_)(obj)),
      "ArraySeq (primitive)"  -> benchmark(ArraySeq.fill(_)(123)),
      "Vector"                -> benchmark(Vector.fill(_)(obj)),
      "List"                  -> benchmark(List.fill(_)(obj)),
      "LazyList"              -> benchmark(LazyList.fill(_)(obj)),
      "Range"                 -> benchmark(Range(0, _)),
      "NumericRange"          -> benchmark(NumericRange(0, _, 1)),
      "HashSet"               -> benchmark(n => scala.collection.immutable.HashSet((1 to n).map(_.toString): _*)),
      "TreeSet"               -> benchmark(n => scala.collection.immutable.TreeSet((1 to n).map(_.toString): _*)),
      "ArrayBuffer"           -> benchmark(ArrayBuffer.fill(_)(obj)),
      "ListBuffer"            -> benchmark(ListBuffer.fill(_)(obj)),
    )

  // We use a format similar to the one used by JMH so that
  // our charts can be generated in the same way
  val report = throw new NotImplementedError("jawn-ast dependency")
  /*
    JArray.fromSeq(
      memories.flatMap { case (name, values) =>
        values.map { case (size, value) =>
          JObject.fromSeq(Seq(
            "benchmark" -> JString(s"$name.memory-footprint"),
            "params" -> JObject.fromSeq(Seq(
              "size" -> JString(size.toString)
            )),
            "primaryMetric" -> JObject.fromSeq(Seq(
              "score" -> JNum(value),
              "scoreConfidence" -> JArray.fromSeq(Seq(JNum(value), JNum(value)))
            ))
          ))
        }
      }.to[Seq]
    )
  Files.write(reportPath, FastRenderer.render(report).getBytes)
  */
}
