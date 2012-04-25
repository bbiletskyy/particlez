package net.particlez.gui.example
import java.awt.Color
import scala.swing.Applet
import scala.swing.Frame
import scala.swing.SimpleSwingApplication
import scala.swing.FlowPanel
import scala.swing.Orientation
import scala.swing.event.WindowClosing
import scala.swing.Label
import net.particlez.gui.ParticleManager
import net.particlez.gui.ExecutionPanel
import javax.swing.UIManager
import net.particlez.ChargedParticle
import net.particlez.BindParticle
import net.particlez.StaticParticle
import net.particlez.FloatingParticle
import net.particlez.SimpleCompound
import net.particlez.UnBindParticle
import net.particlez.Pos

/**
 * This example demonstrates how to implement transport particle that is
 * able to carry some resource through the membrane.
 *
 * There are 3 shortcuts introduced: t,m,r. Where t - transport particle,
 * m - membrane particle, r - resource particle.
 *
 * Transport particle t is able to bind to m or r. Binding restrictions are
 * implemented using several charges.
 */
class MembraneTransportExampleApplet extends Applet {
  object ui extends UI {
    val ep = MembraneTransportExampleApplet.createContent()
    contents = ep
    def init(): Unit = {
      ep.refresh()
    }
  }
}

object MembraneTransportExampleApplet extends SimpleSwingApplication {
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
    case object emmm extends ChargedParticle[Pos]("emmm", 1, -1)
    case object etm extends ChargedParticle[Pos]("etm", 0, -0.25)
    case object ett extends ChargedParticle[Pos]("ett", 0, +1)
    case object emm extends ChargedParticle[Pos]("emm", 0, +1)
    case object err extends ChargedParticle[Pos]("err", 0, +1)
    case object etr extends ChargedParticle[Pos]("etr", 0, -0.25)
    case object c extends SimpleCompound[Pos]("c")
    case object ct extends SimpleCompound[Pos]("ct")
    case object bct extends BindParticle[Pos]("bct", 1, ct(), o)
    case object uct extends UnBindParticle[Pos]("uct", 1, ct(), o)

    val pm = new ParticleManager(o)
    pm.addBasic(f, "floating particle with step = 1")
    pm.addBasic(bct, "creates ct[.] bond")
    pm.addBasic(uct, "destroys ct[.] bond")
    //charges
    pm.addBasic(emmm, "charge that keeps membrane consistency")
    pm.addBasic(etm, "charge supporting binding of t with m")
    pm.addBasic(ett, "charge rejecting binding of t with t")
    pm.addBasic(emm, "charge rejecting binding of m with m")
    pm.addBasic(err, "charge rejecting binding of r with r")
    pm.addBasic(etr, "charge supporting binding of t with r")
    //bonds
    pm.addBond(c(), "default bond")
    pm.addBond(ct(), "transport", Color.blue)
    //shortcuts
    pm.addShortcut("transport", t, Color.blue)
    pm.addShortcut("membrane", m, Color.green)
    pm.addShortcut("resource", r, Color.red)

    def t = c(c(etm), c(ett, ett), c(etr), f, bct, uct)
    def m = c(c(etm), c(emm, emm, emm), c(emmm, emmm))
    def r = c(c(etr), c(err, err, err), f)

    val ep = new ExecutionPanel(pm, 200, 200, 10, 10)
    val membrane = ep.configuration.locations().filter(l => l.coordinates(0) == ep.configuration.limits(0) / 2).map(l => (l -> m)).toMap
    ep.configuration.content ++= membrane
    ep.startStopPanel.setStepsPerFrame(100)
    ep.refresh()
    ep
  }

}
