package net.particlez

import org.junit.Assert._
import org.junit.Test

class PosConfigurationTest {

  @Test
  def testEmptyLocations() {
    case object o extends StaticParticle[Pos]("o")
    case object f extends FloatingParticle[Pos]("f", 2, o)
    val conf = new PosConfiguration(o, 4, 4)
    assert(conf.locations().size == 16)
    assert(conf.locations() == conf.emptyLocations())
    conf.content += (Pos(1, 1) -> f)
    assertEquals(conf.locations().size - 1, conf.emptyLocations().size)
  }

}

class PosIteratorTest {

  @Test
  def testNext() {
    val iter = new PosIterator(1, 1, 1)
    assertTrue(iter.hasNext())
    assertEquals(Pos(0, 0, 0), iter.next())
    assertFalse(iter.hasNext())
  }

  @Test
  def testNextAmount2By2() {
    val iter = new PosIterator(2, 2)
    var i = 0
    while (iter.hasNext()) {
      iter.next()
      i = i + 1
    }
    assertEquals(4, i)
  }

  @Test
  def testNextAmount3By2By5() {
    val iter = new PosIterator(3, 2, 5)
    var i = 0
    while (iter.hasNext()) {
      iter.next()
      i = i + 1
    }
    assertEquals(30, i)
  }

  @Test
  def testHasNext() {
    assertTrue(new PosIterator(1, 1, 1).hasNext())
    try {
      new PosIterator(1, 1, 0)
      fail("Exception is expected, since one element of the provided limits is 0")
    } catch {
      case e: IllegalStateException => //test passed
      case _ => fail("Unexpected exception caught")
    }
  }
}