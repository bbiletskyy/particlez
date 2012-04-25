package net.particlez
import org.junit.Assert._
import org.junit.Test

class StaticParticleTest {

  @Test
  def testTransform() {
    
    case object p extends StaticParticle[Pos]("p")
    //val p = new StaticParticle[Pos]("p")
    val q = p.transform(_ => new StaticParticle[Pos]("q"))
    assertEquals("q", q.name())
  }

  
  //TODO will be removed since equals is not needed
  @Test
  def testEquals() {
    val p = new StaticParticle[Pos]("p")
    assertTrue(p == p)
    val p1 = new StaticParticle[Pos]("p")
    assertTrue(p == p1)
    val q = new StaticParticle[Pos]("q")
    assertFalse(p == q)
    val p2 = new StaticParticle[Pos]("p") { override def radius(): Double = 7 }
    assertFalse(p == q)
    assertFalse(p.equals(q))
  }

  @Test
  def testInterface() {
    val p = new StaticParticle[Pos]("p")
    assertEquals("p", p.name)
    assertEquals(0.0, p.radius, 0.001)
    assertEquals(List(p), p.flatten())
    assertEquals("p", p.toString())
  }
}