package scala.collection.immutable

import org.junit.Test
import org.junit.Assert._
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class OrderedMapXTest {
  @Test
  def t7445(): Unit = {
    val m = OrderedMapX(4 -> 1, 2 -> 2, 1 -> 3, 5 -> 5, 3 -> 4)
    assertEquals(OrderedMapX(2 -> 2, 1 -> 3, 5 -> 5, 3 -> 4), m.tail)
  }
  @Test
  def testBuilder(): Unit = {
    val m = OrderedMapX("d" -> "1", "b" -> "2", "a" -> "3", "e" -> "2.2", "c" -> "4")
    assertEquals(List("d" -> "1", "b" -> "2", "a" -> "3", "e" -> "2.2", "c" -> "4"), m.toList)
  }
  @Test
  def testHeadTailLastInitWhenOrderingByInsertion(): Unit = {
    val m = OrderedMapX(3 -> 1, 2 -> 2, 1 -> 3)
    assertEquals(3 -> 1, m.head)
    assertEquals(OrderedMapX(2 -> 2, 1 -> 3), m.tail)
    assertEquals(1 -> 3, m.last)
    assertEquals(OrderedMapX(3 -> 1, 2 -> 2), m.init)
  }
  @Test
  def testHeadTailLastInitWhenOrderingByModification(): Unit = {
    val m = OrderedMapX(3 -> 1, 2 -> 2, 1 -> 3).orderingBy(OrderedMapX.OrderBy.Modification).updated(2, 4)
    assertEquals(3 -> 1, m.head)
    assertEquals(OrderedMapX(1 -> 3, 2 → 4), m.tail)
    assertEquals(2 -> 4, m.last)
    assertEquals(OrderedMapX(3 -> 1, 1 -> 3), m.init)
  }
  @Test
  def testAddWhenOrderingByInsertion(): Unit = {
    val m = OrderedMapX(3 -> 1, 2 -> 2, 1 -> 3)
    assertEquals(OrderedMapX(3 -> 1, 2 -> 2, 1 -> 3, 4 -> 4), m + (4 -> 4))
    assertEquals(OrderedMapX(3 -> 1, 2 -> 4, 1 -> 3), m + (2 -> 4))
    assertEquals(OrderedMapX(3 -> 1, 2 -> 2, 1 -> 3), m + (2 -> 2))
    assertEquals(OrderedMapX(3 -> 2, 2 -> 2, 1 -> 3), m + (3 -> 2))
  }
  @Test
  def testRemoveWhenOrderingByInsertion(): Unit = {
    val m = OrderedMapX(3 -> 1, 2 -> 2, 1 -> 3)
    assertEquals(OrderedMapX(3 -> 1, 2 -> 2), m - 1)
    assertEquals(OrderedMapX(3 -> 1, 1 -> 3), m - 2)
    assertEquals(OrderedMapX(3 -> 1, 2 -> 2, 1 -> 3), m - 4)
  }
  @Test
  def testAddWhenOrderingByModification(): Unit = {
    val m = OrderedMapX(3 -> 1, 2 -> 2, 1 -> 3).orderingBy(OrderedMapX.OrderBy.Modification)
    assertEquals(OrderedMapX(3 -> 1, 2 -> 2, 1 -> 3, 4 -> 4), m + (4 -> 4))
    assertEquals(OrderedMapX(3 -> 1, 1 -> 3, 2 -> 4), m + (2 -> 4))
    assertEquals(OrderedMapX(3 -> 1, 1 -> 3, 2 -> 2), m + (2 -> 2))
    assertEquals(OrderedMapX(2 -> 2, 3 -> 2, 1 -> 4), m + (3 -> 2) + (1 → 4))
  }
  @Test
  def testRemoveWhenOrderingByModification(): Unit = {
    val m = OrderedMapX(3 -> 1, 2 -> 2, 1 -> 3).orderingBy(OrderedMapX.OrderBy.Modification).updated(3, 3)
    assertEquals(OrderedMapX(2 -> 2, 3 -> 3), m - 1)
    assertEquals(OrderedMapX(1 -> 3, 3 -> 3), m - 2)
    assertEquals(OrderedMapX(2 -> 2, 1 -> 3, 3 -> 3), m - 4)
  }
  @Test
  def testRemoveMultipleWhenOrderingByInsertion(): Unit = {
    val m = OrderedMapX(3 -> 1, 2 -> 2, 1 -> 3, 4 -> 5, 5 -> 4)
    assertEquals(OrderedMapX(3 -> 1, 2 -> 2, 5 -> 4), (m - 1) - 4)
    assertEquals(OrderedMapX(1 -> 3, 5 -> 4), (m - 3) - 2 - 4)
    assertEquals(OrderedMapX(4 -> 5, 5 -> 4), (m - 3) - 1 - 2)
  }
  @Test
  def testRemoveMultipleWhenOrderingByModification(): Unit = {
    val m = OrderedMapX(3 -> 1, 2 -> 2, 1 -> 3, 4 -> 5, 5 -> 4).orderingBy(OrderedMapX.OrderBy.Modification).updated(3, 3)
    assertEquals(OrderedMapX(2 -> 2, 5 -> 4, 3 -> 3), (m - 1) - 4)
    assertEquals(OrderedMapX(1 -> 3, 5 -> 4), (m - 3) - 2 - 4)
    assertEquals(OrderedMapX(4 -> 5, 5 -> 4), (m - 3) - 1 - 2)
  }
  @Test
  def testIterator: Unit = {
    assertEquals(Nil, OrderedMapX.empty.iterator.toList)
    assertEquals(List(4 -> 1), OrderedMapX(4 -> 1).iterator.toList)
    assertEquals(List(4 -> 1, 2 -> 2, 1 -> 3, 5 -> 5, 3 -> 4), OrderedMapX(4 -> 1, 2 -> 2, 1 -> 3, 5 -> 5, 3 -> 4).iterator.toList)
  }
  @Test
  def testRemoveIterator: Unit = {
    val m = OrderedMapX(3 -> 1, 2 -> 2, 1 -> 3, 4 -> 5, 5 -> 4)
    assertEquals(List(3 -> 1, 2 -> 2, 5 -> 4), ((m - 1) - 4).iterator.toList)
    assertEquals(List(1 -> 3, 5 -> 4), ((m - 3) - 2 - 4).iterator.toList)
    assertEquals(List(4 -> 5, 5 -> 4), ((m - 3) - 1 - 2).iterator.toList)
  }
  @Test
  def testSlice: Unit = {
    val m = OrderedMapX(3 -> 1, 2 -> 2, 1 -> 3, 4 -> 5, 5 -> 4)
    assertEquals(List(3 -> 1, 2 -> 2), ((m - 1) - 4).slice(0, 2).iterator.toList)
    assertEquals(List(5 -> 4), ((m - 3) - 2 - 4).slice(1, 2).iterator.toList)
    assertEquals(List(), ((m - 3) - 1 - 2).slice(2, 2).iterator.toList)
    assertEquals(List(), ((m - 3) - 1 - 2).slice(2, 2).iterator.toList)
    assertEquals(List(3 -> 3, 4 -> 4, 5 -> 5), OrderedMapX(1 -> 1, 2 -> 2, 3 -> 3, 4 -> 4, 5 -> 5, 6 -> 6, 7 -> 7).slice(2, 5).iterator.toList)
    assertEquals(List(7 -> 7), OrderedMapX(1 -> 1, 2 -> 2, 3 -> 3, 4 -> 4, 5 -> 5, 6 -> 6, 7 -> 7).slice(6, 7).iterator.toList)
    assertEquals(List(), OrderedMapX(1 -> 1, 2 -> 2, 3 -> 3, 4 -> 4, 5 -> 5, 6 -> 6, 7 -> 7).slice(7, 7).iterator.toList)
  }

  @Test
  def testSplitAt: Unit = {
    val m = OrderedMapX(3 -> 1, 2 -> 2, 1 -> 3, 4 -> 5, 5 -> 4)
    var t = m.splitAt(0)
    assertEquals((List(), List(3 -> 1, 2 -> 2, 1 -> 3, 4 -> 5, 5 -> 4)), (t._1.iterator.toList, t._2.iterator.toList))
    t = m.splitAt(1)
    assertEquals((List(3 -> 1), List(2 -> 2, 1 -> 3, 4 -> 5, 5 -> 4)), (t._1.iterator.toList, t._2.iterator.toList))
    t = m.splitAt(2)
    assertEquals((List(3 -> 1, 2 -> 2), List(1 -> 3, 4 -> 5, 5 -> 4)), (t._1.iterator.toList, t._2.iterator.toList))
    t = m.splitAt(3)
    assertEquals((List(3 -> 1, 2 -> 2, 1 -> 3), List(4 -> 5, 5 -> 4)), (t._1.iterator.toList, t._2.iterator.toList))
    t = m.splitAt(4)
    assertEquals((List(3 -> 1, 2 -> 2, 1 -> 3, 4 -> 5), List(5 -> 4)), (t._1.iterator.toList, t._2.iterator.toList))
    t = m.splitAt(5)
    assertEquals((List(3 -> 1, 2 -> 2, 1 -> 3, 4 -> 5, 5 -> 4), List()), (t._1.iterator.toList, t._2.iterator.toList))
  }
}