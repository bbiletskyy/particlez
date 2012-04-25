package net.particlez
import scala.Array.canBuildFrom


case class ParticleParserException(message: String) extends RuntimeException(message)

class ParticleParser[L <: Distance[L]](val basics: Map[String, Particle[L]], val compounds: Map[String, Compound[L]]) {
  def this(b: Particle[L]*)(c: Compound[L]*) =
    this(Map[String, Particle[L]](b.map(p => (p.name(), p)): _*), Map[String, Compound[L]](c.map(p => (p.name(), p)): _*))
  def this(b: Set[Particle[L]], c: Set[Compound[L]]) = this(b.toSeq: _*)(c.toSeq: _*)

  def apply(s: String): Particle[L] = getParticle(s)

  def getParticle(s: String): Particle[L] = {
    if(s == null || s.isEmpty()) throw new ParticleParserException("Particle name is empty")
    if (s.contains('[')) compound(s) else simple(s)
  }

  private def simple(str: String): Particle[L] = {
    basics.get(str) match {
      case Some(p) => p.transform(pp => pp)
      case None => throw new ParticleParserException("Could not find basic particle corrsponding to name: " + str)
    }
  }
  
  
  private def compound(str: String): Compound[L] = {
    val nestedStart = str.indexWhere('[' == _)
    val nestedEnd = str.lastIndexWhere(']' == _)
    val compoundName = str.substring(0, nestedStart)

    compounds.get(compoundName) match {
      case Some(x) => {
        x(particles(str.substring(nestedStart + 1, nestedEnd)): _*)
      }
      case None => throw new ParticleParserException("Could not find compound particle with name: " + compoundName)
    }
  }

  private def particles(str: String): List[Particle[L]] = {
    if (str.length == 0) return Nil
    val comaIndx = str.findIndexOf(',' == _)
    if (comaIndx == -1) return getParticle(str) :: Nil
    val bracketIndx = str.findIndexOf('[' == _)
    if (bracketIndx == -1) return str.split(',').map(s => getParticle(s)).toList
    if (bracketIndx < comaIndx) {
      val closeBracketIndx = findNestedEnd(str, bracketIndx)
      if (closeBracketIndx + 1 == str.length()) {
        return getParticle(str.substring(0, closeBracketIndx + 1)) :: Nil
      } else {
        return getParticle(str.substring(0, closeBracketIndx + 1)) :: particles(str.substring(closeBracketIndx + 2))
      }
    } else {
      return getParticle(str.substring(0, comaIndx)) :: particles(str.substring(comaIndx + 1))
    }
  }
  private def findNestedEnd(str: String, startIndex: Int): Int = {
    var cntr = 1
    val endNsted = str.indexWhere(c => {
      c match {
        case ']' => cntr = cntr - 1
        case '[' => cntr = cntr + 1
        case _ =>
      }
      cntr == 0
    }, startIndex + 1)
    endNsted
  }

}


object ParticleParser {
  def main(args: Array[String]) {
    case object o extends StaticParticle[Pos]("o")
    case object f extends FloatingParticle[Pos]("f", 2, o)
    case object e extends ChargedParticle[Pos]("e", 2, -1)
    case object b extends SimpleCompound[Pos]("b")
    case object d extends SimpleCompound[Pos]("d")

    assert(new ParticleParser(o, f, e)(b, d)("o") == o)
    assert(new ParticleParser(o, f, e)(b, d)("b[o,f]") == b(o, f))
    assert(new ParticleParser(o, f, e)(b, d)("d[]") == d())
    assert(new ParticleParser(o, f, e)(b, d)("b[d[f,e],o]") == b(d(f, e), o))
    assert(new ParticleParser(o, f, e)(b, d)("b[e,d[],o,d[b[e],o]]") == b(e, d(), o, d(b(e), o)))

  }
}