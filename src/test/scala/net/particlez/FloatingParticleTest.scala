package net.particlez
import org.junit.Assert._
import org.junit.Test



class FloatingParticleTest {
  @Test
  def testInteract() {
    val o = new StaticParticle[Pos]("o")
    val f = new FloatingParticle[Pos]("f", 1, o)
    val self = Pos(10)
    val other = Pos(11)
    val context = Map((self -> f), (other -> o))
    val newContext = f.interact(self, context)
    assert(newContext(other) == f)
  }

  @Test
  def testRadius() {
    val o = new StaticParticle[Pos]("o")
    val f = new FloatingParticle[Pos]("f", 3, o)
    val self = Pos(56)
    val other = Pos(40)
    val context = Map((self -> f), (other -> o))
    val newContext = f.interact(self, context)
    assert(newContext.isEmpty)
  }

  @Test
  def testEmpty() {
    val o = new StaticParticle[Pos]("o")
    val f = new FloatingParticle[Pos]("f", 1, o)
    val x = new StaticParticle[Pos]("x")
    val self = Pos(0)
    val other = Pos(1)
    val context = Map((self -> f), (other -> x))
    val newContext = f.interact(self, context)
    assert(newContext.isEmpty)
  }
 
  @Test
  def testSmth() {
    assertTrue(true)
  }

}