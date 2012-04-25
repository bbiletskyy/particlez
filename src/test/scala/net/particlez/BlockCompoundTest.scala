package net.particlez
import org.junit.Test
import org.junit.Assert._

/**
 * Tests for [[BlockCompound]]
 * 
 * @author Borys Biletskyy
 */
class BlockCompoundTest {

  @Test
  def testBasic() {
    case object o extends StaticParticle[Pos]("o")
    case object x extends StaticParticle[Pos]("x")
    case object f extends StaticParticle[Pos]("f")
    case object c extends BlockCompound[Pos]("c", p => if (p == f) x else p)
    val p = c(o, f, o)
    assertEquals("c[o,f,o]", p.toString())
    assertEquals(List(o, x, o), p.flatten())
    assertEquals(List(o, f, o), p.nested())
  }

  @Test
  def testCompound() {
    case object o extends StaticParticle[Pos]("o")
    case object y extends StaticParticle[Pos]("y")
    case object z extends StaticParticle[Pos]("z")
    case object c extends BlockCompound[Pos]("c", p => if (p == z) y else p)
    val p = c(o, z, c(y, c(y, y)))
    assertEquals(o :: y :: y :: y :: y :: Nil, p.flatten())
  }

  @Test
  def testTransform() {
    case object o extends StaticParticle[Pos]("o")
    case object x extends StaticParticle[Pos]("x")
    case object f extends StaticParticle[Pos]("f")
    case object g extends StaticParticle[Pos]("g")
    case object c extends BlockCompound[Pos]("c", p => if (p == f) x else p)
    val p = c(o, f, o)
    val tp = p.transform(p => if (p == f) g else p).asInstanceOf[Compound[Pos]]
    assertEquals("c[o,g,o]", tp.toString())
    assertEquals(List(o, g, o), tp.flatten())
    assertEquals(List(o, g, o), tp.nested())
  }
}