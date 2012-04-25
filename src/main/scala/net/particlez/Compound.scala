package net.particlez

trait Compound[L <: Distance[L]] extends Particle[L] {
  def nested(): List[Particle[L]]
  def apply(nn: Particle[L]*): Compound[L]
}
