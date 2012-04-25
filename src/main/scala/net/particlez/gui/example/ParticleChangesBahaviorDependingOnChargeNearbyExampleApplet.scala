package net.particlez.gui.example
import scala.swing.event.WindowClosing
import scala.swing.Applet
import scala.swing.Frame
import scala.swing.SimpleSwingApplication
import java.awt.Color

import javax.swing.UIManager
import net.particlez.gui.ExecutionPanel
import net.particlez.gui.ParticleManager
import net.particlez.BlockCompound
import net.particlez.ChargedParticle
import net.particlez.DownParticle
import net.particlez.FloatingParticle
import net.particlez.Pos
import net.particlez.SimpleCompound
import net.particlez.StaticParticle
import net.particlez.UpParticle

/**
 * This example demonstrates how to build a particle that changes its behavior
 * depending on the charges present around. <br/>
 *
 * Imagine given the following particles:<br/>
 * o - empty placeholder
 * f - floating particle, that jumps to the empty placeholder's position located within 1 distance unit away
 * e - charge with value -1 and radius 10
 * m - charge with value +4 and radius 0
 * g - simple compound
 * q - block compound, that substitutes e, m, and f particles with o
 * u - particle that jumps out from compound q
 * d - particle that jumps into compound q
 *
 * We are to create a particle that begins to move when e charges appear in the context.
 *
 * Such particle would be: g(o, q(g(f, m, u, d, e), o))
 */
class ParticleChangesBahaviorDependingOnChargeNearbyExampleApplet extends Applet {
  object ui extends UI {
    val ep = ParticleChangesBahaviorDependingOnChargeNearbyExampleApplet.createContent()
    contents = ep
    def init(): Unit = {
      ep.refresh()
    }
  }
}

object ParticleChangesBahaviorDependingOnChargeNearbyExampleApplet extends SimpleSwingApplication {
  val frame = new Frame {
    UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName())
    contents = createContent()
    reactions += {
      case WindowClosing(_) => System.exit(0)
    }
  }
  def top = frame

  def createContent() = {
    UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName())
    case object o extends StaticParticle[Pos]("o")
    case object f extends FloatingParticle[Pos]("f", 1, o)
    case object s extends ChargedParticle[Pos]("s", 5, +1)
    case object g extends SimpleCompound[Pos]("g")
    //blocks s and f particles
    case object q extends BlockCompound[Pos]("q", p => if (List(s, f).contains(p)) o else p)
    case object u extends UpParticle[Pos]("u", q())
    case object d extends DownParticle[Pos]("d", q())

    val passive = g(q(g(f, s, u, d), o))
    val active = g(g(f, s, u, d), q(o))

    val pm = new ParticleManager(o)
    pm.addBasic(f, "floating particle, r=1")
    pm.addBasic(s, "particle that turns active particle into passive, charged (r=5, c=1)")
    pm.addBasic(u, "jumps out from the bond q")
    pm.addBasic(d, "jumps into the bond q")
    pm.addBond(g, "simple compound", Color.green)
    pm.addBond(q, "block compound, substitutes s or f particle with o", Color.red)
    pm.addShortcut("passive", passive, Color.red)
    pm.addShortcut("active", active, Color.green)

    val ep = new ExecutionPanel(pm, 200, 200, 10, 10)
    for (i <- 1 to 5) ep.addParticleRandomly(passive)
    ep.startStopPanel.setStepsPerFrame(100)
    ep.refresh()
    ep
  }
}
