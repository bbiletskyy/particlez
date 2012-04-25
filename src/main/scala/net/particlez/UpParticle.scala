package net.particlez

/**
 * Particle that jumps 1 level up out from the <code>c</code> compound into the parent compound particle.
 */
class UpParticle[L <: Distance[L]](n: String, c: Compound[L]) extends StaticParticle[L](n) {
  def compound = c
  override def equals(that: Any) = that match {
    case other: UpParticle[L] => other.getClass == getClass && this.name() == other.name() && this.compound == other.compound
    case _ => false
  }
  override def hashCode() = super.hashCode() + compound.hashCode()
  override def interact(self: L, context: Map[L, Particle[L]]): Map[L, Particle[L]] = {
    val res = context(self).transform(p => p match {
      case cc: Compound[L] if cc.nested().exists(pp => {
        pp.name() == c.name && pp.isInstanceOf[Compound[L]] && pp.flatten().exists(this == _)
      }) => {
        val cu = cc.nested().find(pp => pp.name() == compound.name() && pp.isInstanceOf[Compound[L]]).get.asInstanceOf[Compound[L]]
        val u = cu.nested().find(pp => pp.flatten().exists(this == _)).get
        val tcu = cu(cu.nested().diff(u :: Nil): _*)
        val tcc = cc(u :: tcu :: cc.nested().diff(cu :: Nil): _*)
        tcc
      }
      case _ => p
    })
    Map(self -> res)
  }
}

object UpParticle {
  def main(args: Array[String]) {
    println("testing  UpParticle...")
    testInteract1
    testInteract2
    testInteract3
    println("testing  UpParticle OK")
  }

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
    assert(c(u, cu(o), o) == context(self))
  }

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
    assert(context(self) == c(c(u, o), cu(o), o))
  }

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
    assert(context(self) == c(c2(u, o), cu(o), c1(u, o), o))
  }

}