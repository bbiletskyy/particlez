package net.particlez
import scala.util.Random

/**
 * A particle that pushes out the last nested particle out of the compound c to the empty space within active radius.
 * If resulting compound c contains only particle then the the nested particle is used instead of the compound.
 * This interaction is kind of reversed to [[BindParticle]].
 *
 * The action of the particle can be described as
 * (c[x,y],o)->(x,y) when p is true
 * (c[x,y,z],o)->(c[x,y], z) when p is true
 * (c[x,y],o)->(y,x) when p is false
 * (c[x,y,z],o)->(z,c[x,y]) when p is false
 *
 * where x is the current (or containing current) particle and y and z are some other particles.
 *
 * @param n: [[String]] - the name of the particle
 * @param r: [[Double]] - the radius of particle activity
 * @param b: [[Compound[L]]] - the bond to be used for binding
 * @param o: [[Particle[L]]] - empty particle that can not participate in binding
 * @param p: [[Boolean]] - primary, if true then the resulting compound particle is created
 * on this particle's position and current particle appears first among nested
 * particles, if false then then the resulting compound particle is created
 * on that particle's position and current particle appears last among nested
 * particles. Default value is false.
 */
class UnBindParticle[L <: Distance[L]](n: String, r: Double, b: Compound[L], o: Particle[L], p: Boolean = true) extends StaticParticle[L](n) {
  override def radius = r

  override def interact(self: L, context: Map[L, Particle[L]]) = {
    assert(context(self).flatten().exists(this == _))
    val aContext = activeContext(self, context)
    val currEntry = self -> aContext(self)
    val res: Map[L, Particle[L]] = currEntry._2 match {
      case c: Compound[L] if b() == c() && c.flatten().exists(this == _) => {
        val emptyEntries = aContext.toList.diff(currEntry :: Nil).filter(e => o == e._2)
        if (emptyEntries.isEmpty) Map() else {
          val emptyEntry = emptyEntries.toArray.apply(Random.nextInt(emptyEntries.size))
          val pushed = c.nested().last
          val pushing = c.nested().size match{
            case 0 => throw new IllegalStateException("undinding from compound particle with no nested particles")
          	case 1 => o
            case 2 => c.nested().first
            case _ => c(c.nested().take(c.nested().size - 1): _*)
          }
          if (p)
            Map((currEntry._1 -> pushing), (emptyEntry._1 -> pushed))
          else
            Map((currEntry._1 -> pushed), (emptyEntry._1 -> pushing))
        }

      }
      case _ => Map()
    }
    res
  }
}

object UnBindParticle {
  case object o extends StaticParticle[Pos]("o")
  case object f extends FloatingParticle[Pos]("f", 2, o)
  case object c extends SimpleCompound[Pos]("c")
  case object u extends UnBindParticle[Pos]("u", 2, c(), o, false)
  case object uu extends UnBindParticle[Pos]("uu", 2, c(), o, true)

  def main(args: Array[String]) {
    print("Testing UnBindParticle...")
    testUnbindSimple2Nested
    testUnbindSimple2NestedPrimary
    testUnbindSimple3Nested
    testUnbindSimple3NestedPrimary
    testUnbindEmptyContext
    testUnbindNoEmptyInContext
    testUnbindCompoundFromCompoud
    testUnbindCompoundWithSingleNested
    testUnbindOutOfActiveRadius
    println("OK")
  }

  private def testUnbindSimple2Nested() {
    val context = Map((Pos(0) -> c(u, f)), (Pos(1) -> o))
    val res = u.interact(Pos(0), context)
    assert(u == res(Pos(1)))
    assert(f == res(Pos(0)))
  }

  private def testUnbindSimple2NestedPrimary() {
    val context = Map((Pos(0) -> c(uu, f)), (Pos(1) -> o))
    val res = uu.interact(Pos(0), context)
    assert(uu == res(Pos(0)))
    assert(f == res(Pos(1)))
  }

  private def testUnbindSimple3Nested() {
    val context = Map((Pos(0) -> c(f, u, f)), (Pos(1) -> o))
    val res = u.interact(Pos(0), context)
    assert(c(f, u) == res(Pos(1)))
    assert(f == res(Pos(0)))
  }

  private def testUnbindSimple3NestedPrimary() {
    val context = Map((Pos(0) -> c(f, uu, f)), (Pos(1) -> o))
    val res = uu.interact(Pos(0), context)
    assert(c(f, uu) == res(Pos(0)))
    assert(f == res(Pos(1)))
  }

  private def testUnbindEmptyContext() {
    val context = Map((Pos(0) -> c(f, u)))
    val res = u.interact(Pos(0), context)
    assert(res.isEmpty)
  }
  private def testUnbindNoEmptyInContext() {
    val context = Map((Pos(0) -> c(f, u)), (Pos(1) -> f))
    val res = u.interact(Pos(0), context)
    assert(res.isEmpty)
  }

  private def testUnbindCompoundFromCompoud() {
    val context = Map((Pos(0) -> c(f, c(u, f))), (Pos(1) -> o))
    val res = u.interact(Pos(0), context)
    assert(c(u, f) == res(Pos(0)))
  }

  private def testUnbindCompoundWithSingleNested() {
    val context = Map((Pos(0) -> c(u)), (Pos(1) -> o))
    val res = u.interact(Pos(0), context)
    assert(u == res(Pos(0)))
    assert(o == res(Pos(1)))
  }
  
  private def testUnbindOutOfActiveRadius() {
    val context = Map((Pos(0) -> c(u)), (Pos(3) -> o))
    val res = u.interact(Pos(0), context)
    assert(res.isEmpty)
  }

}
