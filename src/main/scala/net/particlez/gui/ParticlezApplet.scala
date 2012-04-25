package net.particlez.gui

import java.awt.Color
import scala.swing._
import scala.swing.event.WindowOpened
import scala.swing.event.WindowClosing
import net.particlez.ChargedParticle
import net.particlez.StaticParticle
import net.particlez.FloatingParticle
import net.particlez.SimpleCompound
import net.particlez.Pos
import javax.swing.UIManager

class ParticlezApplet extends Applet {
  case object o extends StaticParticle[Pos]("o")
  case object f extends FloatingParticle[Pos]("f", 5, o)
  case object e extends ChargedParticle[Pos]("e", 5, -5)
  case object b extends SimpleCompound[Pos]("b")
  case object d extends SimpleCompound[Pos]("d")

  object ui extends UI {
    val ep = createContent()
    contents = ep
    def createContent() = {
      UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName())
      val pm = new ParticleManager(o)
      pm.addBasic(f, "floating particle with step length 5", Color.blue)
      pm.addBasic(e, "charged particle with charge -5", Color.orange)
      pm.addBond(b, "default bond", Color.cyan)
      pm.addBond(d, "default bond", Color.magenta)
      new ExecutionPanel(pm)
    }

    def init(): Unit = {
      println("Init applet")
      ep.configuration.content += (Pos(2, 3) -> f)
      ep.refresh()

    }
  }
}

