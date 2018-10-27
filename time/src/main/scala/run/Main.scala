package run

object Main {
  def main(args: Array[String]): Unit = {
    import scala.collection.immutable._

    println("ArraySeq: " + ArraySeq(1, 2, 3).concat(List(4, 5, 6)))
    println("List: " + List(1, 2, 3).concat(List(4, 5, 6)))
    println("LazyList: " + LazyList(1, 2, 3).concat(List(4, 5, 6)).mkString(","))
    println("Vector: " + Vector(1, 2, 3).concat(List(4, 5, 6)))
    println("BitSet: " + BitSet(1, 2, 3).concat(List(4, 5, 6)))
    println("HashSet: " + HashSet(1, 2, 3).concat(List(4, 5, 6)))
    println("Queue: " + Queue(1, 2, 3).concat(List(4, 5, 6)))
    println("TreeSet: " + TreeSet(1, 2, 3).concat(List(4, 5, 6)))
    println("IntMap: " + IntMap(1 -> 1, 2 -> 2, 3 -> 3).concat(List(4 -> 4, 5 -> 5, 6 -> 6)))
    println("LongMap: " + LongMap(1L -> 1, 2L -> 2, 3L -> 3).concat(List(4L -> 4, 5L -> 5, 6L -> 6)))
    println("HashMap: " + HashMap(1 -> 1, 2 -> 2, 3 -> 3).concat(List(4 -> 4, 5 -> 5, 6 -> 6)))
    println("TreeMap: " + TreeMap(1 -> 1, 2 -> 2, 3 -> 3).concat(List(4 -> 4, 5 -> 5, 6 -> 6)))
    println("VectorMap: " + VectorMap(1 -> 1, 2 -> 2, 3 -> 3).concat(List(4 -> 4, 5 -> 5, 6 -> 6)))
  }
}
