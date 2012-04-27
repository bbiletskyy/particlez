package net.particlez

import scala.util.Random

/**
 * A square shaped configuration where particle locations are described by integer vectors.
 * The top left point of the configuration has coordinates (0,0,...,0) the bottom right point is represented by a limits vector provided.
 * The configuration is filled by the empty particle provided as a parameter. This particle is also used to test whether the particle is active.
 */
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

/** A location in the configuration that is represented by a vector of real numbers */
case class Pos(coordinates: Array[Double]) extends Distance[Pos] {
  val dim: Int = coordinates.size
  def distance(that: Pos): Double = {
    if (this.dim != that.dim) throw new IllegalArgumentException("Wrong dimension")
    var res = 0.0
    for (i <- 0 until dim) res += Math.pow(this.coordinates(i) - that.coordinates(i), 2)
    Math.sqrt(res)
  }

  override def equals(that: Any) = this.getClass() == that.getClass() && coordinates.toList == that.asInstanceOf[Pos].coordinates.toList
  override def hashCode() = coordinates.toList.hashCode()
  override def toString() = coordinates.mkString("Pos(", ";", ")")
}

/** A companion object that brings alternative constructor to case class (case classes can not have multiple constructors). */
object Pos {
  def apply(c: Double*) = new Pos(c.toArray)
}

/** Iterates through all the integer vectors laying between (0,0,...,0) and the limits vector provided. Needed to traverse configuration locations. */
private class PosIterator(val limits: Array[Int]) extends Iterator[Pos] {
  limits.foreach(e => if (e <= 0) throw new IllegalStateException("limits elements must be strictly positive"))
  def this(bounds: Int*) = this(bounds.toArray)
  var nxt = Array.fill(limits.size)(0)
  def hasNext() = !nxt.zip(limits).exists(e => e._1 >= e._2)

  def next(): Pos = {
    if (!hasNext()) throw new RuntimeException("Iterator has no more elements");
    val res = Pos(nxt.map(_.toDouble).toArray)
    inc(0)
    res
  }

  private def inc(i: Int) {
    if (i < limits.size) {
      nxt(i) = nxt(i) + 1
      if (nxt(i) == limits(i)) {
        nxt(i) = 0
        inc(i + 1)
      }
    } else {
      nxt = limits
    }
  }

}


