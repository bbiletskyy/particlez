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

