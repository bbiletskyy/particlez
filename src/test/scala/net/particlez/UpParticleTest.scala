package net.particlez

import org.junit.Test
import org.junit.Assert._

class UpParticleTest {

  @Test
  def testInteract1() {
    case object o extends StaticParticle[Pos]("o")
    case object cu extends SimpleCompound[Pos]("cu")
    case object u extends UpParticle[Pos]("u", cu())
    case object c extends SimpleCompound[Pos]("c")

    val p: Particle[Pos] = c(cu(u, o), o)
    val self = Pos(0)
    var context = Map(self -> p)
    for (i <- 1 to 10) {
      context = context ++ p.interact(self, context)
    }
    assertEquals(c(u, cu(o), o), context(self))
  }

  @Test
  def testInteract2() {
    case object o extends StaticParticle[Pos]("o")
    case object c extends SimpleCompound[Pos]("c")
    case object cu extends SimpleCompound[Pos]("cu")
    case object u extends UpParticle[Pos]("u", cu())

    val cuo = c(u, o)
    val ccuo = cu(cuo, o)
    val p: Particle[Pos] = c(ccuo, o)

    val self = Pos(0)
    var context = Map(self -> p)
    for (i <- 1 to 10) {
      context = context ++ p.interact(self, context)
    }
    assertEquals(c(c(u, o), cu(o), o), context(self))
  }

  @Test
  def testInteract3() {
    case object o extends StaticParticle[Pos]("o")
    case object c extends SimpleCompound[Pos]("c")
    case object c1 extends SimpleCompound[Pos]("c1")
    case object c2 extends SimpleCompound[Pos]("c2")
    case object cu extends SimpleCompound[Pos]("cu")
    case object u extends UpParticle[Pos]("u", cu())

    val p: Particle[Pos] = c(cu(c1(u, o), o, c2(u, o)), o)

    val self = Pos(0)
    var context = Map(self -> p)
    for (i <- 1 to 100) {
      context = context ++ p.interact(self, context)
    }
    assertEquals(c(c2(u, o), cu(o), c1(u, o), o), context(self))
  }
}