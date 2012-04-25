package net.particlez
import scala.Array.canBuildFrom
import scala.annotation.elidable

import annotation.elidable.ASSERTION

class PosIterator(val limits: Array[Int]) extends Iterator[Pos] {
  limits.foreach(e => if(e <= 0 ) throw new IllegalArgumentException("limits elements must be strictly positive"))
  def this(bounds: Int*) = this(bounds.toArray)
  var nxt = Array.fill(limits.size)(0)
  def hasNext() = !nxt.zip(limits).exists(e => e._1 >= e._2)

  def next(): Pos = {
    if (!hasNext()) throw new RuntimeException("Iterator has no more elements");
    val res = Pos(nxt.map(_.toDouble).toList)
    inc(0)
    res
  }

  private def inc(i: Int) {
    if (i < limits.size) {
      nxt(i) = nxt(i) + 1
      if (nxt(i) == limits(i)) {
        nxt(i) = 0
        inc(i + 1)
      }
    } else {
      nxt = limits
    }
  }

}

object PosIterator {
  def main(args: Array[String]) {
    testHasNext()
    testNext()
    testNextAmount2By2()
  }

  def testNext() {
    val iter = new PosIterator(1, 1, 1)
    assert(iter.hasNext() == true)
    assert(iter.next() == Pos(0, 0, 0))
    assert(iter.hasNext() == false)
  }

  def testNextAmount2By2() {
    val iter = new PosIterator(2, 2)
    var i = 0
    while (iter.hasNext()) {
      iter.next()
      i = i + 1
    }
    assert(i == 4)
  }

  def testNextAmount3By2By5() {
    val iter = new PosIterator(2, 2)
    var i = 0
    while (iter.hasNext()) {
      iter.next()
      i = i + 1
    }
    assert(i == 30)
  }
  
  def testHasNext() {
    assert(new PosIterator(1, 1, 1).hasNext() == true)
    try {
      new PosIterator(1, 1, 0)
      assert(false)
    } catch {
      case t: Throwable =>
      case _ => assert(false)
    }

  }

}