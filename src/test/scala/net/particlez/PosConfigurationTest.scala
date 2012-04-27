package net.particlez

import org.junit.Assert._
import org.junit.Test

class PosConfigurationTest {

  @Test
  def testEmptyLocations() {
    case object o extends StaticParticle[Pos]("o")
    case object f extends FloatingParticle[Pos]("f", 2, o)
    val conf = new PosConfiguration(o, 4, 4)
    assert(conf.locations().size == 16)
    assert(conf.locations() == conf.emptyLocations())
    conf.content += (Pos(1, 1) -> f)
    assertEquals(conf.locations().size - 1, conf.emptyLocations().size)
  }

}