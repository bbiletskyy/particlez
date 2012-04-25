package net.particlez
import scala.util.Random

/**
 * A particle that binds to nonempty particle within r distance units using the bond b.
 *
 * The action of the particle can be described as
 * (x,y)->(c[x,y],o) when m is true
 * (x,y)->(o,c[y,x]) when m is false
 *
 * @param n: [[String]] - the name of the particle
 * @param r: [[Double]] - the radius of particle activity
 * @param b: [[Compound[L]]] - the bond to be used for binding
 * @param o: [[Particle[L]]] - empty particle that can not participate in binding
 * @param m: [[Boolean]] - if true then the resulting compound particle is created
 * on this particle's position and current particle appears first among nested
 * particles, if false then then the resulting compound particle is created
 * on that particle's position and current particle appears last among nested
 * particles. Default value is false.
 */
class BindParticle[L <: Distance[L]](n: String, r: Double, b: Compound[L], o: Particle[L], m: Boolean = false) extends StaticParticle[L](n) {
  override def radius = r

  override def interact(self: L, context: Map[L, Particle[L]]) = {
    assert(context(self).flatten().exists(this == _))
    val aContext = activeContext(self, context)
    val currEntry = self -> aContext(self)
    val bindToEntries = (aContext - currEntry._1).filterNot(e => o == e._2)
    if (bindToEntries.isEmpty) Map() else {
      val bindToEntry = bindToEntries.toArray.apply(Random.nextInt(bindToEntries.size))
      if (m)
        Map((currEntry._1 -> b(currEntry._2, bindToEntry._2)), (bindToEntry._1 -> o))
      else
        Map((currEntry._1 -> o), (bindToEntry._1 -> b(bindToEntry._2, currEntry._2)))
    }
  }
}


