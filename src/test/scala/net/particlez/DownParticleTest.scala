package net.particlez
import org.junit.Test
import org.junit.Assert._

/**
 * Tests for [[DownParticle]]
 *
 * @author Borys Biletskyy
 */
class DownParticleTest {
  @Test
  def testInteract1() {
    case object o extends StaticParticle[Pos]("o")
    case object cd extends SimpleCompound[Pos]("cd")
    case object d extends DownParticle[Pos]("d", cd())
    case object ccd extends SimpleCompound[Pos]("ccd")

    val p: Particle[Pos] = ccd(cd(o), o, d)
    val self = Pos(0)
    var context = Map(self -> p)
    for (i <- 1 to 10) {
      context = context ++ p.interact(self, context)
    }
    assertEquals(ccd(cd(d, o), o), context(self))
  }

  @Test
  def testInteract2() {
    case object o extends StaticParticle[Pos]("o")
    case object cd extends SimpleCompound[Pos]("cd")
    case object d extends DownParticle[Pos]("d", cd())
    case object ccd extends SimpleCompound[Pos]("ccd")
    case object cdd extends SimpleCompound[Pos]("cdd")

    val p: Particle[Pos] = ccd(o, cdd(o, d), cd(o))
    val self = Pos(0)
    var context = Map(self -> p)
    for (i <- 1 to 10) {
      context = context ++ p.interact(self, context)
    }

    //assertEquals(ccd(cd(cdd(o, d), o), o), context(self))
    //interaction is stochastic, so there is no way of making sure that it actually happened
    assertTrue(ccd(cd(cdd(o, d), o), o) == context(self) || ccd(o, cdd(o, d), cd(o)) == context(self))
  }

  @Test
  def testInteract3() {
    case object o extends StaticParticle[Pos]("o")
    case object cd extends SimpleCompound[Pos]("cd")
    case object d extends DownParticle[Pos]("d", cd())
    case object ccd extends SimpleCompound[Pos]("ccd")
    case object cdd extends SimpleCompound[Pos]("cdd")

    val p: Particle[Pos] = ccd(o, cd(o), cdd(o, d), cd(o))
    val self = Pos(0)
    var context = Map(self -> p)
    for (i <- 1 to 10) {
      context = context ++ p.interact(self, context)
    }
    //2 transformations are possible, one followed by another one
    assertTrue(ccd(cd(cdd(o, d), o), o, cd(o)) == context(self) || ccd(cd(cd(cdd(o, d), o), o), o) == context(self) || ccd(o, cd(o), cdd(o, d), cd(o)) == context(self))
  }

  @Test
  def testInteract4() {
    case object o extends StaticParticle[Pos]("o")
    case object y extends SimpleCompound[Pos]("y")
    case object d extends DownParticle[Pos]("d", y())
    case object g extends SimpleCompound[Pos]("g")
    val p: Particle[Pos] = g(o, y(d, o))
    val self = Pos(0)
    var context = Map(self -> p)
    for (i <- 1 to 100) {
      context = context ++ p.interact(self, context)
    }
    assertEquals(g(o, y(d, o)), context(self))
  }
}