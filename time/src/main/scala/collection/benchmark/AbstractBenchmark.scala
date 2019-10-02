package collection.benchmark

import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations._

/*
-f 1 -bm avgt -bs 1 -w 200ms -r 500ms -to 30s -wi 5 -i 7 -gc true -jvmArgs -Xmx10G -jvmArgs -Xss4M -jvmArgs -XX:-TieredCompilation -t 1 (HashMapB|ListMapB|OrderedMapX|VectorMapX).+
 */

@BenchmarkMode(scala.Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Benchmark)
@Warmup(iterations = 9, time = 700, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 7, time = 500, timeUnit = TimeUnit.MILLISECONDS)
@Fork(value = 1, jvmArgs = Array(
  "-server",
  "-Xms2g",
  "-Xmx2g",
  "-XX:NewSize=1g",
  "-XX:MaxNewSize=1g",
  "-XX:InitialCodeCacheSize=512m",
  "-XX:ReservedCodeCacheSize=512m",
  "-XX:+AlwaysPreTouch",
  /*
  "-XX:+UnlockExperimentalVMOptions",
  "-XX:+UseJVMCICompiler",
  "-XX:+EnableJVMCI",
  "-XX:+EagerJVMCI",
  "-Dgraal.ShowConfiguration=info",
  "-XX:+UseParallelGC",
  "-XX:-UseBiasedLocking",
  "-XX:-TieredCompilation",*/
))
abstract class AbstractBenchmark {
  def exceptions: PartialFunction[String, Boolean] = PartialFunction.empty

  private val failed = new RuntimeException("excepted")
  failed.setStackTrace(Array.empty[StackTraceElement])

  protected def except(methodName: String): Unit = {
    if (exceptions.applyOrElse(methodName, (_: String) => false)) throw failed
  }
}