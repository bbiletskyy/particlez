package net.particlez


//TO DO Think about changing by a particle that blocks all its contents or charge blocker and interaction blocker

/** Substitutes all occurrences of particle to block <code>b</code> with substitute particle <code>s</code> in flatten */
class BlockCompound[L <: Distance[L]](n: String, b: Particle[L] => Particle[L], nn: List[Particle[L]]) extends SimpleCompound[L](n, nn) {

  def this(n: String, b: Particle[L] => Particle[L], nn: Particle[L]*) = this(n, b, List(nn: _*))

  private def blockedNested = nn.map(_.transform(b))

  override def flatten(): List[Particle[L]] = blockedNested.flatMap(_.flatten())

  def block() = b

  override def equals(that: Any) = that match {
    case other: BlockCompound[L] => getClass == other.getClass && name == other.name &&
      nested == other.nested && block == other.block
    case _ => false
  }

  override def hashCode() = n.hashCode() + b.hashCode() + nn.hashCode()

  //override def apply(nested: List[Particle[L]]): BlockCompound[L] = new BlockCompound[L](n, b, nested)
  override def apply(nested: Particle[L]*): BlockCompound[L] = new BlockCompound(n, b, nested: _*)
}
