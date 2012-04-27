package net.particlez
import org.junit.Test
import org.junit.Assert._

class ChargedParticleTest {
  val DELTA = 0.001

  @Test
  def testRadius() {
    case object e extends ChargedParticle[Pos]("e", 4, -1)
    val self = Pos(0)
    val other = Pos(5)
    val context = Map((self -> e), (other -> e))
    val r = e.evaluate(self, context)
    assertEquals(e.charge, r, DELTA)
  }

  @Test
  def testEvaluateSimpleSimple() {
    case object u extends ChargedParticle[Pos]("u", 1, 1)
    case object v extends ChargedParticle[Pos]("v", 1, 2)
    case object w extends ChargedParticle[Pos]("w", 1, 4)
    case object c extends SimpleCompound[Pos]("c")
    val l = Pos(0)
    var x: Particle[Pos] = u
    var context = Map((l -> x))
    var r = x.evaluate(l, context)
    assertEquals(u.charge(), r, DELTA)

    x = v
    context = Map((l -> x))
    r = x.evaluate(l, context)
    assertEquals(v.charge(), r, DELTA)

    x = w
    context = Map((l -> x))
    r = x.evaluate(l, context)
    assertEquals(w.charge(), r, DELTA)

    x = u
    context = Map((l -> x), (Pos(1) -> u))
    r = x.evaluate(l, context)
    assertEquals(r, context.flatMap(e => e._2.flatten()).count(u == _) * u.charge, DELTA)

    x = v
    context = Map((l -> x), (Pos(1) -> v))
    r = x.evaluate(l, context)
    assertEquals(r, context.flatMap(e => e._2.flatten()).count(v == _) * v.charge, DELTA)

    x = w
    context = Map((l -> x), (Pos(1) -> w))
    r = x.evaluate(l, context)
    assertEquals(r, context.flatMap(e => e._2.flatten()).count(w == _) * w.charge, DELTA)

  }

  @Test
  def testEvaluateCompoundCompound() {
    case object u extends ChargedParticle[Pos]("u", 1, 1)
    case object v extends ChargedParticle[Pos]("v", 1, 2)
    case object w extends ChargedParticle[Pos]("w", 1, 4)
    case object c extends SimpleCompound[Pos]("c")

    val l = Pos(0)
    var x: Particle[Pos] = c(u, u)
    var context = Map((l -> x), (Pos(1) -> c(u, u)))
    var r = x.evaluate(l, context) //8
    assertEquals(x.flatten().count(u == _) * countParticles(context, u) * u.charge, r, DELTA)

    x = c(v, v)
    context = Map((l -> x), (Pos(1) -> c(v, v)))
    r = x.evaluate(l, context) // 16
    assertEquals(x.flatten().count(v == _) * countParticles(context, v) * v.charge, r, DELTA)

    x = c(w, w)
    context = Map((l -> x), (Pos(1) -> c(w, w)))
    r = x.evaluate(l, context) //32
    assertEquals(x.flatten().count(w == _) * countParticles(context, w) * w.charge, r, DELTA)

    x = c(u, u, u, u)
    context = Map((l -> x), (Pos(1) -> c(u, u, u, u)))
    r = x.evaluate(l, context) //32
    assertEquals(x.flatten().count(u == _) * countParticles(context, u) * u.charge, r, DELTA)

    x = c(c(u, u), c(u, u))
    context = Map((l -> x), (Pos(1) -> c(c(u, u), c(u, u))))
    r = x.evaluate(l, context) //32
    assertEquals(x.flatten().count(u == _) * countParticles(context, u) * u.charge, r, DELTA)

  }

  @Test
  def testEvaluateCompound() {
    case object u extends ChargedParticle[Pos]("u", 0, 1)
    case object v extends ChargedParticle[Pos]("v", 0, -2)
    case object w extends ChargedParticle[Pos]("w", 0, 4)
    case object c extends SimpleCompound[Pos]("c")
    val l = Pos(0)
    var x: Particle[Pos] = c(u, u, u, u)
    var context = Map((l -> x))
    var r = x.evaluate(l, context)
    assertEquals(countParticles(x, u) * countParticles(x, u) * u.charge, r, DELTA)
    assertEquals(4 * 4 * 1, r, DELTA)

    x = c(c(u, u), c(u, u))
    context = Map((l -> x))
    r = x.evaluate(l, context) //4*4*1
    assertEquals(countParticles(x, u) * countParticles(x, u) * u.charge, r, DELTA)
    assertEquals(4 * 4 * 1, r, DELTA)

    x = c(c(v, v), c(v, v, v))
    context = Map((l -> x))
    r = x.evaluate(l, context) //5*5*-2
    assertEquals(5 * 5 * -2, r, DELTA)
    assertEquals(countParticles(x, v) * countParticles(x, v) * v.charge, r, DELTA)

    x = c(u, v, w)
    context = Map((l -> x))
    r = x.evaluate(l, context)
    assertEquals(countTetnsion(x, u) + countTetnsion(x, v) + countTetnsion(x, w), r, DELTA)

    x = c(c(u, v, w), c(u, u, v, v))
    context = Map((l -> x))
    r = x.evaluate(l, context)
    assertEquals(countTetnsion(x, u) + countTetnsion(x, v) + countTetnsion(x, w), r, DELTA)

  }

  private def countTetnsion(where: Particle[Pos], p: ChargedParticle[Pos]) = Math.pow(countParticles(where, p), 2) * p.charge()
  private def countParticles(where: Particle[Pos], p: Particle[Pos]) = where.flatten().count(p == _)
  private def countParticles(context: Map[Pos, Particle[Pos]], p: Particle[Pos]) = context.flatMap(e => e._2.flatten()).count(p == _)

}