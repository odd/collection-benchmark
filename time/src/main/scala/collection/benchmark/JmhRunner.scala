package collection.benchmark

import java.io.{File, FilenameFilter, FileOutputStream, PrintStream}
import java.nio.charset.Charset
import java.nio.file.{Files, Paths, StandardOpenOption}
import java.time.{Instant, LocalDateTime, ZonedDateTime}
import java.util
import scala.util.Try
import org.openjdk.jmh.infra.{BenchmarkParams, IterationParams}
import org.openjdk.jmh.results.{BenchmarkResult, IterationResult, RunResult}
import org.openjdk.jmh.results.format.{ResultFormat, ResultFormatType}
import org.openjdk.jmh.runner.{BenchmarkList, BenchmarkListEntry, Defaults, Runner}
import org.openjdk.jmh.runner.format.OutputFormat
import org.openjdk.jmh.runner.options._

object JmhRunner {
  import scala.collection.JavaConverters._

  def main(args: Array[String]): Unit = {
    val opts = new CommandLineOptions(args: _*) // parse command line arguments, and then bend them to your will! ;-)
    val b = new OptionsBuilder()
    b.parent(opts)
    val resultsDir = new File("results")
    resultsDir.mkdir()
    b.result("results/jmh-result-" + Instant.now().toString.replace(':', '-') + ".json")
    val runner = new Runner(b.build())
    runner.run().asScala.toList.groupBy(_.getPrimaryResult.getLabel).foreach {
      case (label, results) =>
        JsonResultFormat.write(new File(resultsDir, s"jmh-benchmark-$label.json"), results.asJava)
    }
    val utf8 = Charset.forName("UTF-8")
    val benchmarks = resultsDir.listFiles(new FilenameFilter {
      override def accept(dir: File, name: String) = name.startsWith("jmh-benchmark-")
    })
    if (benchmarks.nonEmpty) {
      val writer = Files.newBufferedWriter(new File(resultsDir, "jmh-result.json").toPath, utf8)
      try {
        writer.write("[\n")
        var first = true
        benchmarks.foreach { file =>
          val reader = Files.newBufferedReader(file.toPath, utf8)
          try {
            if (first) {
              first = false
            } else {
              writer.write(",\n")
            }
            var line: String = reader.readLine
            while (line != null) {
              if (line != "[" && line != "]") writer.write(line + "\n")
              line = reader.readLine()
            }
          } finally if (reader != null) reader.close()
        }
        writer.write("]\n")
      } finally if (writer != null) writer.close()
    }
  }

  object JsonResultFormat {
    private val jsonResultFormatConstructor = Class.forName("org.openjdk.jmh.results.format.JSONResultFormat").getConstructor(classOf[PrintStream])
    jsonResultFormatConstructor.setAccessible(true)

    def write(file: File, results: java.util.Collection[RunResult]): Unit = {
      val out = new PrintStream(file)
      Try {
        val format: ResultFormat = jsonResultFormatConstructor.newInstance(out).asInstanceOf[ResultFormat]
        format.writeOut(results)
        out.close()
      }
    }
  }
}