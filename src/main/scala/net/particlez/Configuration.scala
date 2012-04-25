package net.particlez

trait Distance[L] {
  def distance(that: L): Double
}

trait Configuration[L <: Distance[L]] {
  def content: scala.collection.mutable.Map[L, Particle[L]]
  def update(): Unit
  def locations(): List[L]
  //allows some optimization by excluding non-active particles from updating process 
  def isActive(p: Particle[L]): Boolean
}
