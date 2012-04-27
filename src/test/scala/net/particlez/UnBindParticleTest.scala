package net.particlez

import org.junit.Test
import org.junit.Assert._

class UnBindParticleTest {

  case object o extends StaticParticle[Pos]("o")
  case object f extends FloatingParticle[Pos]("f", 2, o)
  case object c extends SimpleCompound[Pos]("c")
  case object u extends UnBindParticle[Pos]("u", 2, c(), o, false)
  case object uu extends UnBindParticle[Pos]("uu", 2, c(), o, true)

  @Test
  def testUnbindSimple2Nested() {
    val context = Map((Pos(0) -> c(u, f)), (Pos(1) -> o))
    val res = u.interact(Pos(0), context)
    assertEquals(u, res(Pos(1)))
    assertEquals(f, res(Pos(0)))
  }

  @Test
  def testUnbindSimple2NestedPrimary() {
    val context = Map((Pos(0) -> c(uu, f)), (Pos(1) -> o))
    val res = uu.interact(Pos(0), context)
    assertEquals(uu, res(Pos(0)))
    assertEquals(f, res(Pos(1)))
  }

  @Test
  def testUnbindSimple3Nested() {
    val context = Map((Pos(0) -> c(f, u, f)), (Pos(1) -> o))
    val res = u.interact(Pos(0), context)
    assertEquals(c(f, u), res(Pos(1)))
    assertEquals(f, res(Pos(0)))
  }

  @Test
  def testUnbindSimple3NestedPrimary() {
    val context = Map((Pos(0) -> c(f, uu, f)), (Pos(1) -> o))
    val res = uu.interact(Pos(0), context)
    assertEquals(c(f, uu), res(Pos(0)))
    assertEquals(f, res(Pos(1)))
  }

  @Test
  def testUnbindEmptyContext() {
    val context = Map((Pos(0) -> c(f, u)))
    val res = u.interact(Pos(0), context)
    assertTrue(res.isEmpty)
  }

  @Test
  def testUnbindNoEmptyInContext() {
    val context = Map((Pos(0) -> c(f, u)), (Pos(1) -> f))
    val res = u.interact(Pos(0), context)
    assertTrue(res.isEmpty)
  }

  @Test
  def testUnbindCompoundFromCompoud() {
    val context = Map((Pos(0) -> c(f, c(u, f))), (Pos(1) -> o))
    val res = u.interact(Pos(0), context)
    assertEquals(c(u, f), res(Pos(0)))
  }

  @Test
  def testUnbindCompoundWithSingleNested() {
    val context = Map((Pos(0) -> c(u)), (Pos(1) -> o))
    val res = u.interact(Pos(0), context)
    assertEquals(u, res(Pos(0)))
    assertEquals(o, res(Pos(1)))
  }

  @Test
  def testUnbindOutOfActiveRadius() {
    val context = Map((Pos(0) -> c(u)), (Pos(3) -> o))
    val res = u.interact(Pos(0), context)
    assertTrue(res.isEmpty)
  }
}