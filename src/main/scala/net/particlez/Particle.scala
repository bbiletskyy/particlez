package net.particlez

trait Particle[L <: Distance[L]] {
  //remove parentheses
  def name(): String
  def radius(): Double
  def evaluate(self: L, context: Map[L, Particle[L]]): Double
  def interact(self: L, context: Map[L, Particle[L]]): Map[L, Particle[L]]
  def flatten(): List[Particle[L]]
  def transform(t: Particle[L] => Particle[L]): Particle[L]
  def activeContext(self: L, context: Map[L, Particle[L]]): Map[L, Particle[L]] = context.filter(e => self.distance(e._1) <= radius)
  //TODO add contains(predicate: Particle[L]=>Boolean): Boolean method
  
}
