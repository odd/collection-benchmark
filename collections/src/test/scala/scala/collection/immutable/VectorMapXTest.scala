package scala.collection.immutable

import org.junit.Test
import org.junit.Assert._
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class VectorMapXTest {

  @Test
  def t7445(): Unit = {
    val m = VectorMapX(4 -> 1, 2 -> 2, 1 -> 3, 5 -> 5, 3 -> 4)
    assertEquals(List(2 -> 2, 1 -> 3, 5 -> 5, 3 -> 4), m.tail.toList)
  }

  @Test
  def testBuilder(): Unit = {
    val m = VectorMapX("d" -> "1", "b" -> "2", "a" -> "3", "e" -> "2.2", "c" -> "4")
    assertEquals(List("d" -> "1", "b" -> "2", "a" -> "3", "e" -> "2.2", "c" -> "4"), m.toList)
  }

  @Test
  def testHeadTailLastInit(): Unit = {
    val m = VectorMapX(3 -> 1, 2 -> 2, 1 -> 3)
    assertEquals(3 -> 1, m.head)
    assertEquals(List(2 -> 2, 1 -> 3), m.tail.toList)
    assertEquals(1 -> 3, m.last)
    assertEquals(List(3 -> 1, 2 -> 2), m.init.toList)
  }

  @Test
  def testAdd(): Unit = {
    val m = VectorMapX(3 -> 1, 2 -> 2, 1 -> 3)
    assertEquals(List(3 -> 1, 2 -> 2, 1 -> 3, 4 -> 4), (m + (4 -> 4)).toList)
    assertEquals(List(3 -> 1, 2 -> 4, 1 -> 3), (m + (2 -> 4)).toList)
    assertEquals(List(3 -> 1, 2 -> 2, 1 -> 3), (m + (2 -> 2)).toList)
    assertEquals(List(3 -> 2, 2 -> 2, 1 -> 3), (m + (3 -> 2)).toList)
  }

  @Test
  def testRemove(): Unit = {
    val m = VectorMapX(3 -> 1, 2 -> 2, 1 -> 3)
    assertEquals(List(3 -> 1, 2 -> 2), (m - 1).toList)
    assertEquals(List(3 -> 1, 1 -> 3), (m - 2).toList)
    assertEquals(List(2 -> 2, 1 -> 3), (m - 3).toList)
    assertEquals(List(3 -> 1, 2 -> 2, 1 -> 3), (m - 4).toList)
  }

  @Test
  def testRemoveMultiple(): Unit = {
    val m = VectorMapX(3 -> 1, 2 -> 2, 1 -> 3, 4 -> 5, 5 -> 4)
    assertEquals(List(3 -> 1, 2 -> 2, 5 -> 4), ((m - 1) - 4).toList)
    assertEquals(List(1 -> 3, 5 -> 4), ((m - 3) - 2 - 4).toList)
    assertEquals(List(4 -> 5, 5 -> 4), ((m - 3) - 1 - 2).toList)
  }

  @Test
  def testIterator: Unit = {
    assertEquals(Nil, VectorMapX.empty.iterator.toList)
    assertEquals(List(4 -> 1), VectorMapX(4 -> 1).iterator.toList)
    assertEquals(List(4 -> 1, 2 -> 2, 1 -> 3, 5 -> 5, 3 -> 4), VectorMapX(4 -> 1, 2 -> 2, 1 -> 3, 5 -> 5, 3 -> 4).iterator.toList)
  }

  @Test
  def testRemoveIterator: Unit = {
    val m = VectorMapX(3 -> 1, 2 -> 2, 1 -> 3, 4 -> 5, 5 -> 4)
    assertEquals(List(3 -> 1, 2 -> 2, 5 -> 4), ((m - 1) - 4).iterator.toList)
    assertEquals(List(1 -> 3, 5 -> 4), ((m - 3) - 2 - 4).iterator.toList)
    assertEquals(List(4 -> 5, 5 -> 4), ((m - 3) - 1 - 2).iterator.toList)
    assertEquals(List(3 -> 1, 1 -> 3), ((m - 4) - 5 - 2).iterator.toList)
  }
}
