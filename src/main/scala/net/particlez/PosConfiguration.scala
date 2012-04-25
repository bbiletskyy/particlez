package net.particlez

import scala.util.Random

class PosConfiguration(val o: Particle[Pos], val limits: Double*) extends Configuration[Pos] {
  val beta = 1.0
  val origin = Array.fill(limits.size)(0.0)
  val content = scala.collection.mutable.Map[Pos, Particle[Pos]]()
  fill(o)

  def emptyLocations() = locations().filter(l => content(l) == o)
  def locations() = new PosIterator(limits.map(l => l.toInt).toArray).toList

  def update() {
    val activeLocations = locations.filter(l => isActive(content(l)))
    if (activeLocations.isEmpty) return
    val currLocation = activeLocations(Random.nextInt(activeLocations.size))
    val currParticle = content(currLocation)
    val maxRadius = content.toList.foldLeft(0.0)((r, e) => if (e._2.radius() > r) e._2.radius else r)
    val neighborhood = locations().filter(other => currLocation.distance(other) <= maxRadius * 2).map(l => (l -> content(l))).toMap

    val energyBefore = neighborhood.toList.foldLeft(0.0)((r, e) => r + e._2.evaluate(e._1, neighborhood))
    val updatedEntries = currParticle.interact(currLocation, neighborhood)
    val updatedNeighborhood = neighborhood ++ updatedEntries
    val energyAfter = updatedNeighborhood.toList.foldLeft(0.0)((r, e) => r + e._2.evaluate(e._1, updatedNeighborhood))

    val delta = energyAfter - energyBefore;
    if (delta <= 0) {
      content ++= updatedEntries
    } else if (Random.nextDouble() <= Math.exp(-beta * delta)) {
      content ++= updatedEntries
    }
  }

  //def isActive(p: Particle[Pos]) = true
  def isActive(p: Particle[Pos]) = p != o
  def fill(p: Particle[Pos]): Unit = content ++= locations().map((_ -> p)).toMap
}

case class Pos(coordinates: List[Double]) extends Distance[Pos] {
  def dim(): Int = coordinates.size
  def distance(that: Pos): Double = {
    if (this.dim != that.dim) throw new IllegalArgumentException("Vrong dimension")
    var res = 0.0
    for (i <- 0 until dim()) res += Math.pow(this.coordinates(i) - that.coordinates(i), 2)
    Math.sqrt(res)
  }

  //TODO remove these methods
  //  override def equals(that: Any) = if (this.getClass() == that.getClass() && coordinates.toList == that.asInstanceOf[Pos].coordinates.toList) true else false
  //  override def hashCode() = coordinates.toList.hashCode()
  //  override def toString() = coordinates.mkString("Pos(", ";", ")")
}

object Pos {
  def apply(c: Double*) = new Pos(c.toList)
}

object PosConfiguration {
  def main(args: Array[String]) {
    testEmptyLocations()
    test1d()
    test2d()
    exampleFloating()
    exampleCharged()
  }

  def testEmptyLocations() {
    case object o extends StaticParticle[Pos]("o")
    case object f extends FloatingParticle[Pos]("f", 2, o)
    val conf = new PosConfiguration(o, 4, 4)
    assert(conf.locations().size == 16)
    assert(conf.locations() == conf.emptyLocations())
    conf.content += (Pos(1, 1) -> f)
    assert(conf.locations().size - 1 == conf.emptyLocations().size)
  }

  def exampleCharged() {
    val o = new StaticParticle[Pos]("o")
    val f = new FloatingParticle[Pos]("f", 2, o)
    val e = new ChargedParticle[Pos]("e", 2, +5)
    val g = new SimpleCompound("g")(f, e)
    val c = new PosConfiguration(o, 4, 4)
    c.content += (Pos(0, 0) -> e)
    c.content += (Pos(1, 1) -> g)
    for (i <- 0 to 100) {
      println("Iteration: " + i)
      displayConfig2D(c)
      c.update()
    }
  }

  def exampleFloating() {

    val o = new StaticParticle[Pos]("o")
    val f = new FloatingParticle[Pos]("f", 2, o)
    val c = new PosConfiguration(o, 4, 4)
    val l = c.locations()(Random.nextInt(c.locations.length))
    c.content += (l -> f)

    for (i <- 0 to 100) {
      println("Iteration: " + i)
      displayConfig2D(c)
      c.update()
    }
  }

  def test1d() {
    val o = new StaticParticle[Pos]("o")
    val s = new StaticParticle[Pos]("s")
    val f = new FloatingParticle[Pos]("f", 2, o)
    val c = new PosConfiguration(o, 10)
    val l = c.locations()(Random.nextInt(c.locations.length))
    c.content += (l -> f)
    for (i <- 0 to 100) {
      println("Iteration: " + i)
      displayConfig1D(c)
      c.update()
    }
  }

  def test2d() {
    val o = new StaticParticle[Pos]("o")
    val f = new FloatingParticle[Pos]("f", 2, o)
    val c = new PosConfiguration(o, 4, 4)
    c.fill(o)
    val l = c.locations()(Random.nextInt(c.locations.length))
    c.content += (l -> f)
    for (i <- 0 to 100) {
      println("Iteration: " + i)
      displayConfig2D(c)
      c.update()
    }
  }

  def displayConfig1D(c: PosConfiguration) {
    if (c.limits.size != 1) throw new IllegalArgumentException("configuration is not 1D one")
    val particles = for (i <- 0.0 until (c.limits.head, 1.0)) yield c.content(Pos(i))
    val str = particles.map(p => p.name()).mkString("[", "-", "]")
    println(str)
  }

  def displayConfig2D(c: PosConfiguration) {
    if (c.limits.size != 2) throw new IllegalArgumentException("configuration is not 1D one")

    val rows = for (y <- 0.0 until (c.limits(1), 1)) yield {
      val row = for (x <- 0.0 until (c.limits(0), 1)) yield {
        c.content(Pos(x, y)).name()
      }
      row.mkString("|", " ", "|")
    }
    println(rows.mkString("\n"))
  }
}