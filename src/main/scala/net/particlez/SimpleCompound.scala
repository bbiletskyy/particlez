package net.particlez
import scala.util.Random

class SimpleCompound[L <: Distance[L]](val n: String, nn: List[Particle[L]]) extends StaticParticle[L](n) with Compound[L] {
  //TODO modify constructors to allign with case objects extending this class
  def this(n: String, nn: Particle[L]*) = this(n, List(nn: _*))
  //from Particle
  override def radius(): Double = {
    val nestedRadiusList = flatten().map(_.radius())
    if (nestedRadiusList.isEmpty) 0 else nestedRadiusList.max
  }

  override def evaluate(o: L, context: Map[L, Particle[L]]): Double = flatten().foldLeft(0.0) { _ + _.evaluate(o, context) }
  override def interact(o: L, context: Map[L, Particle[L]]): Map[L, Particle[L]] = {
    if (flatten().isEmpty) Map() else flatten()(Random.nextInt(flatten().size)).interact(o, context)
  }
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

object SimpleCompound {
  def main(args: Array[String]) {
    println("testing SimpleCompound...")
    testSetNested()
    testTransform()
    testTransformCompound()
    testBasic()
    testHashCode()
    testEquals()
    testInteract()
    testInteractEmpty()
    testEvaluate()
    testEvaluateEmpty()
    testRadius()
    testRadiusEmptyNested()
    println("OK")
  }

  def testBasic() {
    val p = new StaticParticle[Pos]("p")
    val q = new StaticParticle[Pos]("q") { override def radius() = 1 }
    val c = new SimpleCompound[Pos]("c", p, q)
    //name
    assert(c.name() == "c")
    assert(c.toString() == "c[p,q]")
    //flatten
    assert(c.flatten() == List(p, q))
    //radius
    assert(c.radius() == q.radius())
  }

  def testHashCode() {
    val p = new StaticParticle[Pos]("p")
    val q = new StaticParticle[Pos]("q")
    assert(new SimpleCompound[Pos]("c1")(p, p).hashCode() == new SimpleCompound[Pos]("c1", p, p).hashCode())
    assert(new SimpleCompound[Pos]("c1", p, p).hashCode() != new SimpleCompound[Pos]("c1", p, q).hashCode())
    assert(new SimpleCompound[Pos]("c2", p, p).hashCode() != new SimpleCompound[Pos]("c1", p, p).hashCode())
    assert(new SimpleCompound[Pos]("c1", p, p).hashCode() == new SimpleCompound[Pos]("c1", p, p) { override def radius() = 7 }.hashCode())
  }

  def testEquals() {
    val p = new StaticParticle[Pos]("p")
    val q = new StaticParticle[Pos]("q")
    val c = new SimpleCompound[Pos]("c", p, p)

    val c2 = new SimpleCompound[Pos]("c", p, p)
    assert(c.equals(c2))
    assert(c == c2)
    val c3 = new SimpleCompound[Pos]("c", p, q)
    assert(c != c3)
    val c4 = new SimpleCompound[Pos]("c", p, p, p)
    assert(c != c4)
    val c5 = new SimpleCompound[Pos]("c", p, p) { override def radius(): Double = 7 }
    assert(c != c5)
  }

  def testSetNested() {
    val p = new StaticParticle[Pos]("p")
    val q = new StaticParticle[Pos]("q")
    val c = new SimpleCompound[Pos]("c", p, q)
    val cc = c(q, q)
    assert(c.toString() == "c[p,q]")
    assert(cc.toString() == "c[q,q]")
  }

  def testTransform() {
    val p = new StaticParticle[Pos]("p")
    val q = new StaticParticle[Pos]("q")
    val c = new SimpleCompound[Pos]("c", p, q)
    assert(c.toString() == "c[p,q]")
    val x = new StaticParticle[Pos]("x")
    val res = c.transform(n => if (n == p) x else n)
    assert(res.toString() == "c[x,q]")
  }

  def testTransformCompound() {
    val p = new StaticParticle[Pos]("p")
    val q = new StaticParticle[Pos]("q")
    val c1 = new SimpleCompound[Pos]("c1", p, q)
    val c2 = new SimpleCompound[Pos]("c2", c1, q)
    assert(c2.toString() == "c2[c1[p,q],q]")
    val res = c2.transform(pp => if (pp == new SimpleCompound[Pos]("c1", p, q)) p else pp)
    assert(res.toString() == "c2[p,q]")
  }

  def testInteract() {
    val q = new StaticParticle[Pos]("q") {
      override def interact(self: Pos, context: Map[Pos, Particle[Pos]]): Map[Pos, Particle[Pos]] = Map((self -> this))
    }
    val c = new SimpleCompound[Pos]("c", q, q)
    val self = Pos(0)
    val context = Map(Pos(0) -> c)
    val res = c.interact(self, context)
    assert(res.equals(Map(self -> q)))
  }

   def testInteractEmpty() {
    val c = new SimpleCompound[Pos]("c")
    val self = Pos(0)
    val context = Map(Pos(0) -> c)
    val res = c.interact(self, context)
    assert(res.equals(Map()))
  }
  
  
  def testEvaluate() {
    val p = new StaticParticle[Pos]("p") {
      override def evaluate(self: Pos, context: Map[Pos, Particle[Pos]]): Double = 7
    }
    val q = new StaticParticle[Pos]("q") {
      override def evaluate(self: Pos, context: Map[Pos, Particle[Pos]]): Double = 1
    }
    val c = new SimpleCompound("c", p, q)
    val self = Pos(0)
    val context = Map(Pos(0) -> c)
    val res = c.evaluate(self, context)
    assert(res.toInt == 8)
  }
  
  def testEvaluateEmpty() {
    val c = new SimpleCompound[Pos]("c")
    val self = Pos(0)
    val context = Map(Pos(0) -> c)
    val res = c.evaluate(self, context)
    assert(res.toInt == 0)
  }


  def testRadius() {
    val o = new StaticParticle[Pos]("o")
    val f = new FloatingParticle[Pos]("f", 7, o)
    val c = new SimpleCompound[Pos]("c", o, f)
    assert(c.radius() == f.radius())
  }

  def testRadiusEmptyNested() {
    assert(new SimpleCompound[Pos]("c").radius() == 0)
  }

}