/*
package scala.collection.immutable

import scala.reflect.ClassTag

sealed trait Chain[+A, V <: AnyVal]
    extends IndexedSeq[A]
    //with IndexedSeqOps[A, Chain, Chain[A, V]]
    //with StrictOptimizedSeqOps[A, Chain, Chain[A, V]]
    with Serializable {
  def linkage: Chain.Linkage[A, V]
}
object Chain {
  trait Linker[A, V <: AnyVal] {
    def step: Int
    def link(elem: A, array: Array[V], index: Int): Unit
    def unlink(array: Array[V], index: Int): A
  }
  def empty[A, V <: AnyVal] = Empty.asInstanceOf[Chain[A, V]]
  case object Empty extends Chain[Nothing, Nothing] {
    override def length = 0
    override def linkage = Linkage.Empty
  }
  case class Filled[+A, V <: AnyVal: ClassTag] private (
      linkage: Linkage[A, V],
      lastIndex: Int)(implicit linker: Linker[A, V])
      extends Chain[A, V] {
    override val length = lastIndex / linker.step
  }
  type Link[V <: AnyVal] = Array[V]
  val Link = Array
  sealed trait Linkage[+A, V] extends /*IndexedSeq[Link[V]]
      with IndexedSeqOps[V, Linkage, Link[V]]
      with StrictOptimizedSeqOps[V, Linkage, Link[V]]
      with*/ Serializable {
    def length: Int
    def chained[B >: A, W >: V <: AnyVal]: Chain[B, W]
    def filter(f: Link[V] => Boolean): Linkage[A, V]
    def map[B, W](f: Link[V] => Link[W])(
        implicit linker: Linker[B, W]): Linkage[B, W]
    def flatMap[B, W](f: Link[V] => Linkage[B, W])(
        implicit linker: Linker[B, W]): Linkage[B, W]
  }
  object Linkage {
    def apply[A, V <: AnyVal](elems: A*)(
        implicit linker: Linker[A, V]): Linkage[A, V] = {
      if (elems.knownSize == 0) empty
      else {
        val step = linker.step
        var sz = (if (elems.knownSize > 0) elems.knownSize else 8) * step
        val arr = new Array[V](sz)
        var i = 0
        var iter = elems.iterator
        while (i < sz && iter.hasNext) {
          val elem = iter.next()
          linker.link(elem, arr, i)
          i += step
        }
        Linked(arr, i)
      }
    }
    def empty[A, V <: AnyVal] = Empty.asInstanceOf[Linkage[A, V]]
    case object Empty extends Linkage[Nothing, Nothing] {
      def length = 0
      override def chained[B >: Nothing, Y >: Nothing] = Chain.Empty
      override def filter(f: Link[Nothing] => Boolean) = empty
      override def map[B, W <: AnyVal](f: Link[Nothing] => Link[W])(
          implicit linker: Linker[B, W]) = empty
      override def flatMap[B, W <: AnyVal](f: Link[Nothing] => Linkage[B, W])(
          implicit linker: Linker[B, W]) = empty
    }
    case class Linked[+A, V <: AnyVal: ClassTag] private (
        array: Array[V],
        lastIndex: Int)(implicit linker: Linker[A, V])
        extends Linkage[A, V] {
      val step = linker.step
      val length = lastIndex / step
      def filter(f: Link[V] => Boolean): Linkage[A, V] = {
        var sz = 8
        var arr = new Array[V](sz)
        val link = new Link[V](step)
        var i = 0
        var j = 0
        while (i < lastIndex) {
          read(array, i, link, step)
          if (f(link)) {
            while (j + step < sz) {
              sz *= 2
              val copy = new Array[V](sz)
              Array.copy(arr, 0, copy, 0, j)
              arr = copy
            }
            Array.copy(link, 0, arr, j, step)
            j += step
          }
          i += step
        }
        if (j == 0) empty
        else Linked(arr, j)
      }
      def map[B, W <: AnyVal](f: Link[V] => Link[W])(
          implicit linker: Linker[B, W]): Linkage[B, W] = {
        val arr = new Array[W](lastIndex)
        val link = new Link[V](step)
        var i = 0
        while (i < lastIndex) {
          read(array, i, link, step)
          val mapped = f(link)
          write(arr, i, mapped, step)
          i += step
        }
        Linked(arr, i)
      }
      def flatMap[B, W <: AnyVal](f: Array[V] => Linkage[B, W])(
          implicit linker: Linker[B, W]): Linkage[B, W] = {
        var sz = 8
        val arr = new Array[W](sz)
        val link = new Link[V](step)
        var i = 0
        while (i < lastIndex) {
          read(array, i, link, step)
          f(link) match {
            case Empty =>
            case Linked(arr2, j) =>
              write(arr, i, arr2, step)
              i += step
          }
        }
        Linked(arr, i)
      }
    }
    private def read[V](array: Array[V],
                        index: Int,
                        link: Link[V],
                        step: Int): Unit = {
      var i = index
      var j = 0
      while (j < step) {
        link(j) = array(i)
        i += 1
        j += 1
      }
    }
    private def write[V](array: Array[V],
                         index: Int,
                         link: Array[V],
                         step: Int): Unit = {
      var i = index
      var j = 0
      while (j < step) {
        array(i) = link(j)
        i += 1
        j += 1
      }
    }
  }
  /*
  class Band[V, A](implicit linker: Linker[V, A]) {
    val length =
    def map[U](f: V => U): Linkage[U, A] = {
      linker.link()
    }
  }*/
}
 */
