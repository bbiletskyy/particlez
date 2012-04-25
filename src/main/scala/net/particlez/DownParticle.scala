package net.particlez

/**
 * Particle that jumps 1 level down from the parent compound that contains both this particle and
 * compound <code>c</code> into the <code>c</code> compound particle.
 */
class DownParticle[L <: Distance[L]](n: String, c: Compound[L]) extends StaticParticle[L](n) {

  def compound = c

  override def equals(that: Any) = that match {
    case other: DownParticle[L] => other.getClass == getClass && this.name() == other.name() && this.compound == other.compound
    case _ => false
  }
  override def hashCode() = n.hashCode() + c.hashCode()

  override def interact(self: L, context: Map[L, Particle[L]]): Map[L, Particle[L]] = {
    val res = context(self).transform(p => p match {
      case ccd: Compound[L] if ccd.nested().exists(pp => pp.flatten().exists(this == _) &&
        ccd.nested().diff(List(pp)).exists(ppp => ppp.name == this.compound.name && ppp.isInstanceOf[Compound[L]])) => {
        val d = ccd.nested().find(pp => pp.flatten().exists(this == _)).get
        val cd = ccd.nested().diff(List(d)).find(pp => pp.name() == this.compound.name && pp.isInstanceOf[Compound[L]]).get.asInstanceOf[Compound[L]]
        val tcd = cd(d :: cd.nested: _*)
        ccd(tcd :: ccd.nested().diff(List(d)).diff(List(cd)): _*)
      }
      case _ => p
    })
    Map(self -> res)
  }
}

