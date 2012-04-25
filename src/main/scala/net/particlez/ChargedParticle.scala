package net.particlez

/**
 * Accumulates tension in context, adding charge value
 */
class ChargedParticle[L <: Distance[L]](n: String, r: Double, c: Double) extends StaticParticle[L](n) {
  override def radius() = r
  def charge(): Double = c

  override def evaluate(self: L, context: Map[L, Particle[L]]): Double = {
    assert(context(self).flatten().exists(_ == this))
    def getCharge(p: Particle[L]): Double = p.flatten().count(this == _) * charge()

    val affectedContext = context.filter(e => self.distance(e._1) <= radius)
    val charges = affectedContext.toList.map(e => getCharge(e._2))
    val absCurrCharge = 1.0
    charges.foldLeft(0.0)((total, curr) => total + curr * absCurrCharge)
  }

  override def equals(that: Any) = that match {
    case other: ChargedParticle[L] => getClass == other.getClass && n == other.name &&
      r == other.radius && c == other.charge
    case _ => false
  }

  override def hashCode() = n.hashCode() + r.hashCode() + c.hashCode()
}

object ChargedParticle {
  def main(args: Array[String]) {
    println("Testing ChargedParticle...")
    testRadius
    testEvaluateSimpleSimple
    testEvaluateCompoundCompound
    testEvaluateCompound
    println("OK")
  }

  def testRadius() {
    case object e extends ChargedParticle[Pos]("e", 4, -1)
    val self = Pos(0)
    val other = Pos(5)
    val context = Map((self -> e), (other -> e))
    val r = e.evaluate(self, context)
    assert(r == e.charge)
  }

  private def testEvaluateSimpleSimple() {
    case object u extends ChargedParticle[Pos]("u", 1, 1)
    case object v extends ChargedParticle[Pos]("v", 1, 2)
    case object w extends ChargedParticle[Pos]("w", 1, 4)
    case object c extends SimpleCompound[Pos]("c")
    val l = Pos(0)
    var x: Particle[Pos] = u
    var context = Map((l -> x))
    var r = x.evaluate(l, context)
    assert(r == u.charge())

    x = v
    context = Map((l -> x))
    r = x.evaluate(l, context)
    assert(r == v.charge())

    x = w
    context = Map((l -> x))
    r = x.evaluate(l, context)
    assert(r == w.charge())

    x = u
    context = Map((l -> x), (Pos(1) -> u))
    r = x.evaluate(l, context)
    assert(r == context.flatMap(e => e._2.flatten()).count(u == _) * u.charge)

    x = v
    context = Map((l -> x), (Pos(1) -> v))
    r = x.evaluate(l, context)
    assert(r == context.flatMap(e => e._2.flatten()).count(v == _) * v.charge)

    x = w
    context = Map((l -> x), (Pos(1) -> w))
    r = x.evaluate(l, context)
    assert(r == context.flatMap(e => e._2.flatten()).count(w == _) * w.charge)

  }

  private def testEvaluateCompoundCompound() {
    case object u extends ChargedParticle[Pos]("u", 1, 1)
    case object v extends ChargedParticle[Pos]("v", 1, 2)
    case object w extends ChargedParticle[Pos]("w", 1, 4)
    case object c extends SimpleCompound[Pos]("c")

    val l = Pos(0)
    var x: Particle[Pos] = c(u, u)
    var context = Map((l -> x), (Pos(1) -> c(u, u)))
    var r = x.evaluate(l, context) //8
    assert(r == x.flatten().count(u == _) * countParticles(context, u) * u.charge)

    x = c(v, v)
    context = Map((l -> x), (Pos(1) -> c(v, v)))
    r = x.evaluate(l, context) // 16
    assert(r == x.flatten().count(v == _) * countParticles(context, v) * v.charge)

    x = c(w, w)
    context = Map((l -> x), (Pos(1) -> c(w, w)))
    r = x.evaluate(l, context) //32
    assert(r == x.flatten().count(w == _) * countParticles(context, w) * w.charge)

    x = c(u, u, u, u)
    context = Map((l -> x), (Pos(1) -> c(u, u, u, u)))
    r = x.evaluate(l, context) //32
    assert(r == x.flatten().count(u == _) * countParticles(context, u) * u.charge)

    x = c(c(u, u), c(u, u))
    context = Map((l -> x), (Pos(1) -> c(c(u, u), c(u, u))))
    r = x.evaluate(l, context) //32
    assert(r == x.flatten().count(u == _) * countParticles(context, u) * u.charge)

  }

  private def testEvaluateCompound() {
    case object u extends ChargedParticle[Pos]("u", 0, 1)
    case object v extends ChargedParticle[Pos]("v", 0, -2)
    case object w extends ChargedParticle[Pos]("w", 0, 4)
    case object c extends SimpleCompound[Pos]("c")
    val l = Pos(0)
    var x: Particle[Pos] = c(u, u, u, u)
    var context = Map((l -> x))
    var r = x.evaluate(l, context)
    assert(r == countParticles(x, u) * countParticles(x, u) * u.charge)
    assert(r == 4 * 4 * 1)

    x = c(c(u, u), c(u, u))
    context = Map((l -> x))
    r = x.evaluate(l, context) //4*4*1
    assert(r == countParticles(x, u) * countParticles(x, u) * u.charge)
    assert(r == 4 * 4 * 1)

    x = c(c(v, v), c(v, v, v))
    context = Map((l -> x))
    r = x.evaluate(l, context) //5*5*-2
    assert(r == 5 * 5 * -2)
    assert(r == countParticles(x, v) * countParticles(x, v) * v.charge)

    x = c(u, v, w)
    context = Map((l -> x))
    r = x.evaluate(l, context)
    assert(r == countTetnsion(x, u) + countTetnsion(x, v) + countTetnsion(x, w))

    x = c(c(u, v, w), c(u, u, v, v))
    context = Map((l -> x))
    r = x.evaluate(l, context)
    assert(r == countTetnsion(x, u) + countTetnsion(x, v) + countTetnsion(x, w))

  }

  private def countTetnsion(where: Particle[Pos], p: ChargedParticle[Pos]) = Math.pow(countParticles(where, p), 2) * p.charge()
  private def countParticles(where: Particle[Pos], p: Particle[Pos]) = where.flatten().count(p == _)
  private def countParticles(context: Map[Pos, Particle[Pos]], p: Particle[Pos]) = context.flatMap(e => e._2.flatten()).count(p == _)

}