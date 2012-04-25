package net.particlez


class StaticParticle[L <: Distance[L]](n: String) extends Particle[L] {
  def name(): String = n
  def radius(): Double = 0
  def evaluate(o: L, context: Map[L, Particle[L]]): Double = 0
  def interact(o: L, context: Map[L, Particle[L]]): Map[L, Particle[L]] = Map()
  def flatten(): List[Particle[L]] = this :: Nil
  def transform(t: Particle[L] => Particle[L]): Particle[L] = t(this)
  override def toString(): String = name();
  override def equals(that: Any) = that match {
    case other: StaticParticle[L] => this.getClass == other.getClass && this.name() == other.name()
    case _ => false
  }
  override def hashCode() = n.hashCode()
}

