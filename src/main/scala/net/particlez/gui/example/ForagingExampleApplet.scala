package net.particlez.gui.example
import java.awt.Color
import scala.swing.Applet
import scala.swing.Frame
import scala.swing.SimpleSwingApplication
import scala.swing.FlowPanel
import scala.swing.Orientation
import scala.swing.event.WindowClosing
import scala.swing.Label
import javax.swing.UIManager
import net.particlez.BlockCompound
import net.particlez.ChargedParticle
import net.particlez.FloatingParticle
import net.particlez.SimpleCompound
import net.particlez.StaticParticle
import net.particlez.UpParticle
import net.particlez.DownParticle
import net.particlez.BindParticle
import net.particlez.UnBindParticle
import net.particlez.Pos
import net.particlez.ChargedParticle

import net.particlez.gui.ParticleManager
import net.particlez.gui.ExecutionPanel

/**
 * This example demonstrates how to implement particle foraging using other particles.
 * 
 * There are 2 shortcuts introduced: r, t, where 
 * r - some resource needed to be gathered in one place;
 * t - the transport particle that gather r's together.
 */
class ForagingExampleApplet extends Applet {
  object ui extends UI {
    val ep = ForagingExampleApplet.createContent()
    contents = ep
    def init(): Unit = {
      ep.refresh()
    }
  }
}

object ForagingExampleApplet extends SimpleSwingApplication {
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
    case object f extends FloatingParticle[Pos]("f", 2, o)
    case object er extends ChargedParticle[Pos]("er", 2, -1)
    case object ett extends ChargedParticle[Pos]("ett", 0, 1)
    case object err extends ChargedParticle[Pos]("err", 0, 1)
    case object c extends SimpleCompound[Pos]("c")
    case object ct extends SimpleCompound[Pos]("ct")
    case object bct extends BindParticle[Pos]("bct", 3, ct(), o, true)
    case object uct extends UnBindParticle[Pos]("uct", 3, ct(), o, true)

    def t = c(f, c(ett, ett), bct, uct)
    def r = c(er, c(err, err))

    val pm = new ParticleManager(o)
    pm.addBasic(f, "floating particle with step " + f.radius())
    pm.addBasic(er, "resource charge")
    pm.addBasic(err, "charge preventing r binds to r")
    pm.addBasic(ett, "charge preventing t binds to t")
    pm.addBasic(bct, "creates ct[.] bond")
    pm.addBasic(uct, "destroys ct[.] bond")
    pm.addBond(ct, "binds t with r", Color.blue)
    pm.addBond(c, "default bond")
    pm.addShortcut("transport", t, Color.blue)
    pm.addShortcut("resource", r, Color.red)

    val ep = new ExecutionPanel(pm, 200, 200, 10, 10)
    for (i <- 1 to 10) ep.addParticleRandomly(r)
    for (i <- 1 to 5) ep.addParticleRandomly(t)
    
    ep.startStopPanel.setStepsPerFrame(100)
    ep.refresh()
    ep
  }

}
