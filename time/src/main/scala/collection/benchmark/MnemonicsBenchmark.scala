package collection.benchmark

import scala.collection.immutable.ArraySeq
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

class MnemonicsBenchmark extends AbstractBenchmark {
  @Param(scala.Array("Array", "ArraySeq", "List", "Vector"))
  var impl: String = _
  val dict: List[String] = List("Scala", "rocks", "Pack", "brocks", "GWT", "implicit", "nice", "ScalaGWT", "cat", "EFPL",
    "Lausanne","sCala", "ROcks", "pAck", "Java", "Apple", "Google", "Rochester", "Utah", "Rice", "wyr", "lxm",
    "q", "w", "e", "r", "t", "y", "u", "i","o", "p", "a", "s", "d", "f"
  )

  val mnemonics = Map('2' -> "ABC", '3' -> "DEF", '4' -> "GHI", '5' -> "JKL", '6' -> "MNO", '7' -> "PQRS", '8' -> "TUV", '9' -> "WXYZ")

  def empty: scala.collection.Seq[String] = {
    impl match {
      case "Array" => Array[String]()
      case "ArraySeq" => ArraySeq()
      case "List" => List()
      case "Vector" => Vector()
    }
  }

  @Benchmark
  def mnemonics(bh: Blackhole): Unit = {

    val charCode: Map[Char, Char] =
      for ((digit, str) <- mnemonics; ltr <- str) yield ltr -> digit

    def wordCode(word: String): String = word.toUpperCase.map(charCode)

    val wordsForNum: Map[String, scala.collection.Seq[String]] =
      dict.groupBy(wordCode).withDefaultValue(empty)

    def encode(number: String): Set[scala.collection.Seq[String]] =
      if (number.isEmpty)
        Set(empty)
      else {
        for {
          splitPoint <- Range.inclusive(1, number.length)
          word <- wordsForNum(number take splitPoint)
          rest <- encode(number drop splitPoint)
        } yield word +: rest
      }.toSet

    bh.consume(encode("72252762577225276257528249849874238824").map(_.mkString(" ")))
  }
}