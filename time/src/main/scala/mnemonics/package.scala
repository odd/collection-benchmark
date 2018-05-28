package mnemonics

import java.util.concurrent.TimeUnit
import scala.collection.immutable
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

// run these benchmarks with the following sbt task
// > timeBenchmark/jmh:run mnemonics

@BenchmarkMode(scala.Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@Fork(1)
@Warmup(iterations = 8)
@Measurement(iterations = 8)
@State(Scope.Benchmark)
class List {

  val dict: immutable.List[String] = List("Scala", "rocks", "Pack", "brocks", "GWT", "implicit", "nice", "ScalaGWT", "cat", "EFPL",
    "Lausanne","sCala", "ROcks", "pAck", "Java", "Apple", "Google", "Rochester", "Utah", "Rice", "wyr", "lxm",
    "q", "w", "e", "r", "t", "y", "u", "i","o", "p", "a", "s", "d", "f"
  )

  val mnemonics = immutable.Map('2' -> "ABC", '3' -> "DEF", '4' -> "GHI", '5' -> "JKL", '6' -> "MNO", '7' -> "PQRS", '8' -> "TUV", '9' -> "WXYZ")

  @Benchmark
  def translateNumber(bh: Blackhole): Unit = {

    val charCode: immutable.Map[Char, Char] =
      for ((digit, str) <- mnemonics; ltr <- str) yield ltr -> digit

    def wordCode(word: String): String = word.toUpperCase.map(charCode)

    val wordsForNum: immutable.Map[String, immutable.List[String]] =
      dict.groupBy(wordCode).withDefaultValue(immutable.Nil)

    def encode(number: String): immutable.Set[immutable.List[String]] =
      if (number.isEmpty)
        immutable.Set(immutable.List())
      else {
        for {
          splitPoint <- immutable.Range.inclusive(1, number.length)
          word <- wordsForNum(number take splitPoint)
          rest <- encode(number drop splitPoint)
        } yield word :: rest
      }.toSet

    bh.consume(encode("72252762577225276257528249849874238824").map(_.mkString(" ")))
  }

}

@BenchmarkMode(scala.Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@Fork(1)
@Warmup(iterations = 8)
@Measurement(iterations = 8)
@State(Scope.Benchmark)
class Vector {

  val dict: immutable.Vector[String] = immutable.Vector("Scala", "rocks", "Pack", "brocks", "GWT", "implicit", "nice", "ScalaGWT", "cat", "EFPL",
    "Lausanne","sCala", "ROcks", "pAck", "Java", "Apple", "Google", "Rochester", "Utah", "Rice", "wyr", "lxm",
    "q", "w", "e", "r", "t", "y", "u", "i","o", "p", "a", "s", "d", "f"
  )

  val mnemonics = immutable.Map('2' -> "ABC", '3' -> "DEF", '4' -> "GHI", '5' -> "JKL", '6' -> "MNO", '7' -> "PQRS", '8' -> "TUV", '9' -> "WXYZ")

  @Benchmark
  def translateNumber(bh: Blackhole): Unit = {

    val charCode: immutable.Map[Char, Char] =
      for ((digit, str) <- mnemonics; ltr <- str) yield ltr -> digit

    def wordCode(word: String): String = word.toUpperCase.map(charCode)

    val wordsForNum: immutable.Map[String, immutable.Vector[String]] =
      dict.groupBy(wordCode).withDefaultValue(immutable.Vector.empty)

    def encode(number: String): immutable.Set[immutable.Vector[String]] =
      if (number.isEmpty)
        immutable.Set(immutable.Vector())
      else {
        for {
          splitPoint <- immutable.Range.inclusive(1, number.length)
          word <- wordsForNum(number take splitPoint)
          rest <- encode(number drop splitPoint)
        } yield word +: rest
      }.toSet

    bh.consume(encode("72252762577225276257528249849874238824").map(_.mkString(" ")))
  }

}

@BenchmarkMode(scala.Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@Fork(1)
@Warmup(iterations = 8)
@Measurement(iterations = 8)
@State(Scope.Benchmark)
class ArraySeq {

  val dict: immutable.ArraySeq[String] = immutable.ArraySeq("Scala", "rocks", "Pack", "brocks", "GWT", "implicit", "nice", "ScalaGWT", "cat", "EFPL",
    "Lausanne","sCala", "ROcks", "pAck", "Java", "Apple", "Google", "Rochester", "Utah", "Rice", "wyr", "lxm",
    "q", "w", "e", "r", "t", "y", "u", "i","o", "p", "a", "s", "d", "f"
  )

  val mnemonics = immutable.Map('2' -> "ABC", '3' -> "DEF", '4' -> "GHI", '5' -> "JKL", '6' -> "MNO", '7' -> "PQRS", '8' -> "TUV", '9' -> "WXYZ")

  @Benchmark
  def translateNumber(bh: Blackhole): Unit = {

    val charCode: immutable.Map[Char, Char] =
      for ((digit, str) <- mnemonics; ltr <- str) yield ltr -> digit

    def wordCode(word: String): String = word.toUpperCase.map(charCode)

    val wordsForNum: immutable.Map[String, immutable.ArraySeq[String]] =
      dict.groupBy(wordCode).withDefaultValue(immutable.ArraySeq.empty)

    def encode(number: String): immutable.Set[immutable.ArraySeq[String]] =
      if (number.isEmpty)
        immutable.Set(immutable.ArraySeq())
      else {
        for {
          splitPoint <- immutable.Range.inclusive(1, number.length)
          word <- wordsForNum(number take splitPoint)
          rest <- encode(number drop splitPoint)
        } yield word +: rest
      }.toSet

    bh.consume(encode("72252762577225276257528249849874238824").map(_.mkString(" ")))
  }
}

@BenchmarkMode(scala.Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@Fork(1)
@Warmup(iterations = 8)
@Measurement(iterations = 8)
@State(Scope.Benchmark)
class Array {

  val dict: scala.Array[String] = scala.Array("Scala", "rocks", "Pack", "brocks", "GWT", "implicit", "nice", "ScalaGWT", "cat", "EFPL",
    "Lausanne","sCala", "ROcks", "pAck", "Java", "Apple", "Google", "Rochester", "Utah", "Rice", "wyr", "lxm",
    "q", "w", "e", "r", "t", "y", "u", "i","o", "p", "a", "s", "d", "f"
  )

  val mnemonics = immutable.Map('2' -> "ABC", '3' -> "DEF", '4' -> "GHI", '5' -> "JKL", '6' -> "MNO", '7' -> "PQRS", '8' -> "TUV", '9' -> "WXYZ")

  @Benchmark
  def translateNumber(bh: Blackhole): Unit = {

    val charCode: immutable.Map[Char, Char] =
      for ((digit, str) <- mnemonics; ltr <- str) yield ltr -> digit

    def wordCode(word: String): String = word.toUpperCase.map(charCode)

    val wordsForNum: immutable.Map[String, scala.Array[String]] =
      dict.groupBy(wordCode).withDefaultValue(scala.Array.empty)

    def encode(number: String): immutable.Set[scala.Array[String]] =
      if (number.isEmpty)
        immutable.Set(scala.Array())
      else {
        for {
          splitPoint <- immutable.Range.inclusive(1, number.length)
          word <- wordsForNum(number take splitPoint)
          rest <- encode(number drop splitPoint)
        } yield word +: rest
      }.toSet

    bh.consume(encode("72252762577225276257528249849874238824").map(_.mkString(" ")))
  }
}

/*
@BenchmarkMode(scala.Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@Fork(1)
@Warmup(iterations = 8)
@Measurement(iterations = 8)
@State(Scope.Benchmark)
class Block {

  val dict: immutable.Block[String] = immutable.Block("Scala", "rocks", "Pack", "brocks", "GWT", "implicit", "nice", "ScalaGWT", "cat", "EFPL",
    "Lausanne","sCala", "ROcks", "pAck", "Java", "Apple", "Google", "Rochester", "Utah", "Rice", "wyr", "lxm",
    "q", "w", "e", "r", "t", "y", "u", "i","o", "p", "a", "s", "d", "f"
  )

  val mnemonics = immutable.Map('2' -> "ABC", '3' -> "DEF", '4' -> "GHI", '5' -> "JKL", '6' -> "MNO", '7' -> "PQRS", '8' -> "TUV", '9' -> "WXYZ")

  @Benchmark
  def translateNumber(bh: Blackhole) = {

    val charCode: immutable.Map[Char, Char] =
      for ((digit, str) <- mnemonics; ltr <- str) yield ltr -> digit

    def wordCode(word: String): String = word.toUpperCase.map(charCode)

    val wordsForNum: immutable.Map[String, immutable.Block[String]] =
      dict.groupBy(wordCode).withDefaultValue(immutable.Block.empty)

    def encode(number: String): immutable.Set[immutable.Block[String]] =
      if (number.isEmpty)
        immutable.Set(immutable.Block())
      else {
        for {
          splitPoint <- immutable.Range.inclusive(1, number.length)
          word <- wordsForNum(number take splitPoint)
          rest <- encode(number drop splitPoint)
        } yield word +: rest
      }.toSet

    bh.consume(encode("72252762577225276257528249849874238824").map(_.mkString(" ")))
  }

}
*/