package net.particlez

import scala.util.Random

class FloatingParticle[L <: Distance[L]](n: String, r: Double, e: Particle[L]) extends StaticParticle[L](n) {
  override def radius(): Double = r
  def empty(): Particle[L] = e

  override def interact(self: L, context: Map[L, Particle[L]]): Map[L, Particle[L]] = {
    if (!context(self).flatten().exists(this == _)) {
      println("context: " + context + ", self: " + self)
    }
    assert(context(self).flatten().exists(this == _))
    val emptyEntries = context.toList.filter(e => self.distance(e._1) <= radius()).filter(_._2 == e)
    if (!emptyEntries.isEmpty) {
      val emptyEntry = emptyEntries(Random.nextInt(emptyEntries.size))
      Map((self -> emptyEntry._2), (emptyEntry._1 -> context(self)))
    } else Map()
  }

  //remove this method
  override def equals(that: Any) = that match {
    case other: FloatingParticle[L] => this.name == other.name && this.radius() == other.radius() && this.empty() == other.empty()
    case _ => false
  }
  override def hashCode() = n.hashCode() - r.hashCode() + e.hashCode()

}

