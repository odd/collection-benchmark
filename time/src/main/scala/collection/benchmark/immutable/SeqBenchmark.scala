package collection.benchmark
package immutable

import scala.annotation.tailrec
import scala.collection.immutable._
import scala.collection.mutable
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

class SeqBenchmark extends AbstractIterableBenchmark {
  @Param(scala.Array("ArraySeq", "Vector")) //LazyList", "List", "NumericRange", "Vector"))
  var impl: String = _
  var zipped: Seq[(Long, Long)] = _
  var xs: Seq[Long] = _
  var ys: Seq[Long] = _
  var zs: Seq[Long] = _
  def create(size: Int = 0): Seq[Long] = {
    impl match {
      case "ArraySeq" => ArraySeq(0L until size: _*)
      case "LazyList" => LazyList(0L until size: _*)
      case "List" => List(0L until size: _*)
      case "NumericRange" => 0L until size
      case "Vector" => Vector(0L until size: _*)
    }
  }
  def createBuilder: mutable.Builder[Long, Seq[Long]] = {
    impl match {
      case "ArraySeq" => ArraySeq.newBuilder[Long]
      case "LazyList" => LazyList.newBuilder[Long]
      case "List" => List.newBuilder[Long]
      case "NumericRange" => ???
      case "Vector" => Vector.newBuilder[Long]
    }
  }
  override def zero: Long = 0L
  override def successor = _ + 1L
  override def mapper = -_
  override def combiner = _ + _
  override def spanner = _ < size / 2L
  override def grouper = _ % 5L
  override def finder = n => n > size / 2L

  @Setup(Level.Trial)
  def initTrial(): Unit = {
    xs = create(size)
    ys = xs.updated(xs.size - 1 , 0L)
    zs = create((size / 1000).min(2))
    zipped = xs.map(n => (n, -n))
    if (size > 0) {
      randomIndices = scala.Array.fill(1000)(random.nextInt(size))
    }
  }

  @Benchmark
  def create_build(bh: Blackhole): Unit = {
    except("create_build")
    var t = zero
    val b = createBuilder
    b.sizeHint(size)
    var i = 0
    while (i < size) {
      b.addOne(t)
      t = successor(t)
      i += 1
    }
    bh.consume(b.result())
  }

  @Benchmark
  def expand_foldAppend(bh: Blackhole): Unit = {
    except("expand_foldAppend")
    val ys = (1L to size).foldLeft(create()) {
      case (acc, i) =>
        acc :+ i
    }
    bh.consume(ys)
  }

  @Benchmark
  def expand_foldAppendDouble(bh: Blackhole): Unit = {
    except("expand_foldAppendDouble")
    val ys = (1L to size).foldLeft(create()) {
      case (acc, i) =>
        bh.consume(acc :+ 42L)
        acc :+ i
    }
    bh.consume(ys)
  }

  @Benchmark
  def expand_foldAppendDoubleForeach(bh: Blackhole): Unit = {
    except("expand_foldAppendDoubleForeach")
    val ys = (1L to size).foldLeft(create()) {
      case (acc, i) =>
        bh.consume(acc :+ 42L)
        acc :+ i
    }
    ys.foreach(bh.consume)
  }

  @Benchmark
  def extract_palindrome(bh: Blackhole): Unit = {
    except("extract_palindrome")
    @tailrec def isPalindrome[A](xs: Seq[A]): Boolean = {
      xs match {
        case first +: middle :+ last => first == last && isPalindrome(middle)
        case _ => true
      }
    }
    bh.consume(isPalindrome(xs ++ xs.reverse))
  }

  @Benchmark
  @OperationsPerInvocation(1000)
  def expand_prepend(bh: Blackhole): Unit = {
    except("expand_prepend")
    var ys = xs
    var i = 0L
    while (i < 1000) {
      ys = i +: ys
      i += 1
    }
    bh.consume(ys)
  }

  @Benchmark
  @OperationsPerInvocation(1000)
  def expand_prependTail(bh: Blackhole): Unit = {
    except("expand_prependTail")
    var ys = xs
    var i = 0L
    while (i < 1000) {
      ys = i +: ys
      i += 1
      ys = ys.tail
    }
    bh.consume(ys)
  }

  @Benchmark
  @OperationsPerInvocation(1000)
  def expand_append(bh: Blackhole): Unit = {
    except("expand_append")
    var ys = xs
    var i = 0L
    while (i < 1000) {
      ys = ys :+ i
      i += 1
    }
    bh.consume(ys)
  }

  @Benchmark
  @OperationsPerInvocation(1000)
  def expand_appendInit(bh: Blackhole): Unit = {
    except("expand_appendInit")
    var ys = xs
    var i = 0L
    while (i < 1000) {
      ys = ys :+ i
      i += 1
      ys = ys.init
    }
    bh.consume(ys)
  }

  @Benchmark
  @OperationsPerInvocation(1000)
  def expand_prependAppend(bh: Blackhole): Unit = {
    except("expand_prependAppend")
    var ys = xs
    var i = 0L
    while (i < 1000) {
      if ((i & 1) == 1) ys = ys :+ i
      else ys = i +: ys
      i += 1
    }
    bh.consume(ys)
  }

  @Benchmark
  @OperationsPerInvocation(1000)
  def expand_prependAll(bh: Blackhole): Unit = {
    except("expand_prependAll")
    var ys = xs
    var i = 0L
    while (i < 1000) {
      ys = zs ++: ys
      i += 1
    }
    bh.consume(ys)
  }

  @Benchmark
  @OperationsPerInvocation(1000)
  def expand_appendAll(bh: Blackhole): Unit = {
    except("expand_appendAll")
    var ys = xs
    var i = 0L
    while (i < 1000) {
      ys = ys :++ zs
      i += 1
    }
    bh.consume(ys)
  }

  @Benchmark
  @OperationsPerInvocation(1000)
  def expand_prependAllAppendAll(bh: Blackhole): Unit = {
    except("expand_prependAllAppendAll")
    var ys = xs
    var i = 0L
    while (i < 1000) {
      if ((i & 1) == 1) ys = ys :++ zs
      else ys = zs ++: ys
      i += 1
    }
    bh.consume(ys)
  }

  @Benchmark
  def expand_padTo(bh: Blackhole): Unit = {
    except("expand_padTo")
    bh.consume(xs.padTo(size * 2, 42L))
  }

  @Benchmark
  def traverse_indexed(bh: Blackhole): Unit = {
    except("traverse_indexed")
    var i = 0
    val sz = xs.size
    while (i < sz) {
      bh.consume(mapper(xs(i)))
      i += 1
    }
    bh.consume(i)
  }

  @Benchmark
  @OperationsPerInvocation(1000)
  def access_random(bh: Blackhole): Unit = {
    except("access_random")
    var i = 0
    while (i < 1000) {
      bh.consume(xs(randomIndices(i)))
      i += 1
    }
    bh.consume(i)
  }

  @Benchmark
  @OperationsPerInvocation(100)
  def access_indexOfSlice(bh: Blackhole): Unit = {
    except("access_indexOfSlice")
    var i = 0
    var n = 0L
    while (i < 100) {
      n += xs.indexOfSlice(xs.drop(i))
      i += 1
    }
    bh.consume(n)
  }

  @Benchmark
  @OperationsPerInvocation(1000)
  def transform_updateLast(bh: Blackhole): Unit = {
    except("transform_updateLast")
    var i = 0L
    while (i < 1000) {
      bh.consume(xs.updated(size - 1, i))
      i += 1
    }
    bh.consume(i)
  }

  @Benchmark
  @OperationsPerInvocation(1000)
  def transform_updateForeach(bh: Blackhole): Unit = {
    except("transform_updateForeach")
    var ys = xs
    var i = 0
    while (i < 1000) {
      ys = ys.updated(randomIndices(i), i.toLong)
      bh.consume(ys)
      i += 1
    }
    ys.foreach(bh.consume)
  }

  @Benchmark
  @OperationsPerInvocation(1000)
  def transform_updateRandom(bh: Blackhole): Unit = {
    except("transform_updateRandom")
    var ys = xs
    var i = 0
    while (i < 1000) {
      ys = ys.updated(randomIndices(i), i.toLong)
      bh.consume(ys)
      i += 1
    }
    bh.consume(ys)
  }

  @Benchmark
  @OperationsPerInvocation(100)
  def transform_patch(bh: Blackhole): Unit = {
    except("transform_patch")
    var i = 0
    while (i < 100) {
      val from = randomIndices(i)
      val replaced = randomIndices(if (i > 0) i - 1 else math.min(i + 1, size - 1))
      val length = randomIndices(if (i > 1) i - 2 else math.min(i + 2, size - 1))
      bh.consume(xs.patch(from, xs.take(length), replaced))
      i += 1
    }
    bh.consume(i)
  }

  @Benchmark
  def transform_distinct(bh: Blackhole): Unit = {
    except("transform_distinct")
    bh.consume(xs.distinct)
  }

  @Benchmark
  def transform_distinctBy(bh: Blackhole): Unit = {
    except("transform_distinctBy")
    bh.consume(xs.distinctBy(_ % 2L))
  }

  @Benchmark
  def transform_reverse(bh: Blackhole): Unit = {
    except("transform_reverse")
    bh.consume(xs.reverse)
  }

  @Benchmark
  def transform_lazyZip(bh: Blackhole): Unit = {
    except("transform_lazyZip")
    bh.consume(xs.lazyZip(xs).map((_, _)))
  }

  @Benchmark
  def transform_unzip(bh: Blackhole): Unit = {
    except("transform_unzip")
    bh.consume(zipped.unzip)
  }

}