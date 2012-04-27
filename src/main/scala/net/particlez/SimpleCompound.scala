package net.particlez
import scala.util.Random

/**
 * Composite particle that contains of multiple nested particles. 
 * 
 * The radius, interaction and charge are calculated depending on the corresponding attributes of the nested particles.
 */
class SimpleCompound[L <: Distance[L]](val n: String, nn: List[Particle[L]]) extends StaticParticle[L](n) with Compound[L] {
  //TODO modify constructors to allign with case objects extending this class
  def this(n: String, nn: Particle[L]*) = this(n, List(nn: _*))
  //from Particle
  override def radius(): Double = if(nested.isEmpty) 0 else flatten().map(_.radius()).max

  override def evaluate(o: L, context: Map[L, Particle[L]]): Double = flatten().foldLeft(0.0) { _ + _.evaluate(o, context) }
  override def interact(o: L, context: Map[L, Particle[L]]): Map[L, Particle[L]] = if (flatten().isEmpty) Map() else flatten()(Random.nextInt(flatten().size)).interact(o, context)
  
  override def flatten(): List[Particle[L]] = nested().flatMap(_.flatten())
  override def transform(t: Particle[L] => Particle[L]): Particle[L] = t(this.apply(nested().map(_.transform(t)): _*))
  //from Compound
  def nested(): List[Particle[L]] = nn
  def apply(nested: Particle[L]*): Compound[L] = new SimpleCompound(n, nested: _*)
  //From AnyRef
  override def equals(that: Any) = that match {
    case other: SimpleCompound[L] => this.getClass == that.getClass && this.name() == other.name() && this.nested() == other.nested()
    case _ => false
  }
  override def hashCode() = name().hashCode() + nested().hashCode()
  override def toString(): String = nested().foldLeft(new StringBuilder(name() + "["))((sb, p) => sb.append(p.toString()).append(",")).append("]").toString().replace(",]", "]")
}

