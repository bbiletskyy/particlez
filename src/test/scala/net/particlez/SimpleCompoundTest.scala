package net.particlez

import org.junit.Assert._
import org.junit.Test

class SimpleCompoundTest {

  @Test
  def testBasic() {
    val p = new StaticParticle[Pos]("p")
    val q = new StaticParticle[Pos]("q") { override def radius() = 1 }
    val c = new SimpleCompound[Pos]("c", p, q)

    assertEquals("c", c.name())
    assertEquals("c[p,q]", c.toString())
    assertEquals(List(p, q), c.flatten())
    assertEquals(q.radius.toInt, c.radius.toInt)
  }

  @Test
  def testHashCode() {
    val p = new StaticParticle[Pos]("p")
    val q = new StaticParticle[Pos]("q")
    assertTrue(new SimpleCompound[Pos]("c1")(p, p).hashCode() == new SimpleCompound[Pos]("c1", p, p).hashCode())
    assertFalse(new SimpleCompound[Pos]("c1", p, p).hashCode() == new SimpleCompound[Pos]("c1", p, q).hashCode())
    assertFalse(new SimpleCompound[Pos]("c2", p, p).hashCode() == new SimpleCompound[Pos]("c1", p, p).hashCode())
    assertTrue(new SimpleCompound[Pos]("c1", p, p).hashCode() == new SimpleCompound[Pos]("c1", p, p) { override def radius() = 7 }.hashCode())
  }

  @Test
  def testEquals() {
    val p = new StaticParticle[Pos]("p")
    val q = new StaticParticle[Pos]("q")
    val c = new SimpleCompound[Pos]("c", p, p)

    val c2 = new SimpleCompound[Pos]("c", p, p)
    assertTrue(c.equals(c2))
    assertTrue(c == c2)
    val c3 = new SimpleCompound[Pos]("c", p, q)
    assertFalse(c == c3)
    val c4 = new SimpleCompound[Pos]("c", p, p, p)
    assertFalse(c == c4)
    val c5 = new SimpleCompound[Pos]("c", p, p) { override def radius(): Double = 7 }
    assertFalse(c == c5)
  }

  @Test
  def testSetNested() {
    val p = new StaticParticle[Pos]("p")
    val q = new StaticParticle[Pos]("q")
    val c = new SimpleCompound[Pos]("c", p, q)
    val cc = c(q, q)
    assertEquals("c[p,q]", c.toString)
    assertEquals("c[q,q]", cc.toString)
  }

  @Test
  def testTransform() {
    val p = new StaticParticle[Pos]("p")
    val q = new StaticParticle[Pos]("q")
    val c = new SimpleCompound[Pos]("c", p, q)
    assertEquals("c[p,q]", c.toString)
    val x = new StaticParticle[Pos]("x")
    val res = c.transform(n => if (n == p) x else n)
    assertEquals("c[x,q]", res.toString)
  }

  @Test
  def testTransformCompound() {
    val p = new StaticParticle[Pos]("p")
    val q = new StaticParticle[Pos]("q")
    val c1 = new SimpleCompound[Pos]("c1", p, q)
    val c2 = new SimpleCompound[Pos]("c2", c1, q)
    assertEquals("c2[c1[p,q],q]", c2.toString)
    val res = c2.transform(pp => if (pp == new SimpleCompound[Pos]("c1", p, q)) p else pp)
    assertEquals("c2[p,q]", res.toString)
  }

  @Test
  def testInteract() {
    val q = new StaticParticle[Pos]("q") {
      override def interact(self: Pos, context: Map[Pos, Particle[Pos]]): Map[Pos, Particle[Pos]] = Map((self -> this))
    }
    val c = new SimpleCompound[Pos]("c", q, q)
    val self = Pos(0)
    val context = Map(Pos(0) -> c)
    val res = c.interact(self, context)
    assertEquals(Map(self -> q), res)
  }

  @Test
  def testInteractEmpty() {
    val c = new SimpleCompound[Pos]("c")
    val self = Pos(0)
    val context = Map(Pos(0) -> c)
    val res = c.interact(self, context)
    assertEquals(Map(), res)
  }

  @Test
  def testEvaluate() {
    val p = new StaticParticle[Pos]("p") {
      override def evaluate(self: Pos, context: Map[Pos, Particle[Pos]]): Double = 7
    }
    val q = new StaticParticle[Pos]("q") {
      override def evaluate(self: Pos, context: Map[Pos, Particle[Pos]]): Double = 1
    }
    val c = new SimpleCompound("c", p, q)
    val self = Pos(0)
    val context = Map(Pos(0) -> c)
    val res = c.evaluate(self, context)
    assertEquals(8, res.toInt)
  }

  @Test
  def testEvaluateEmpty() {
    val c = new SimpleCompound[Pos]("c")
    val self = Pos(0)
    val context = Map(Pos(0) -> c)
    val res = c.evaluate(self, context)
    assertEquals(0, res.toInt)
  }

  @Test
  def testRadius() {
    val o = new StaticParticle[Pos]("o")
    val f = new FloatingParticle[Pos]("f", 7, o)
    val c = new SimpleCompound[Pos]("c", o, f)
    assertEquals(f.radius.toInt, c.radius.toInt)
  }

  @Test
  def testRadiusEmptyNested() {
    assertEquals(0, new SimpleCompound[Pos]("c").radius.toInt )
  }
}