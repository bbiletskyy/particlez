package net.particlez
import org.junit.Assert._
import org.junit.Test

class BindParticleTest {
  case object o extends StaticParticle[Pos]("o")
  case object c extends SimpleCompound[Pos]("c")
  case object f extends FloatingParticle[Pos]("f", 1, o)
  case object b extends BindParticle[Pos]("b", 2, c(), o)
  case object bb extends BindParticle[Pos]("bb", 2, c(), o, true)

  @Test
  def testDoesNotBindOutsideRadius() {
    val context = Map((Pos(0) -> b), (Pos(3) -> f))
    val res = b.interact(Pos(0), context)
    assertTrue(res.isEmpty)
  }

  @Test
  def testSimpleBindsToSimple() {
    val context = Map((Pos(0) -> b), (Pos(1) -> f))
    val res = b.interact(Pos(0), context)(Pos(1))
    assertEquals(c(f, b), res)
  }

  @Test
  def testSimpleBindsToSimplePrimaryMode() {
    val context = Map((Pos(0) -> bb), (Pos(1) -> f))
    val res = bb.interact(Pos(0), context)
    assertEquals(c(bb, f), res(Pos(0)))
    assertEquals(o, res(Pos(1)))
  }

  @Test
  def testSimpleBindsToCompound() {
    val context = Map((Pos(0) -> b), (Pos(1) -> c(f, f)))
    val res = b.interact(Pos(0), context)(Pos(1))
    assertEquals(c(c(f, f), b), res)
  }

  @Test
  def testCompoundBindsToSimple() {
    val context = Map((Pos(0) -> c(b)), (Pos(1) -> f))
    val res = b.interact(Pos(0), context)(Pos(1))
    assertEquals(c(f, c(b)), res)
  }

  @Test
  def testCompoundBindsToCompound() {
    val context = Map((Pos(0) -> c(b)), (Pos(1) -> c(f, f)))
    val res = b.interact(Pos(0), context)(Pos(1))
    assertEquals(c(c(f, f), c(b)), res)
  }

  @Test
  def testCompoundDoesNotBindToItself() {
    val context = Map((Pos(0) -> c(b)))
    val res = b.interact(Pos(0), context)
    assertTrue(res.isEmpty)
  }

  @Test
  def testDoesNotBindToEmptyContext() {
    val context = Map((Pos(0) -> b))
    val res = b.interact(Pos(0), context)
    assertTrue(res.isEmpty)
  }
}