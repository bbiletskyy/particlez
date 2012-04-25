package net.particlez.gui.example
import java.awt.Color
import scala.swing.event.WindowClosing
import scala.swing.Applet
import scala.swing.Frame
import scala.swing.SimpleSwingApplication
import javax.swing.UIManager
import net.particlez.FloatingParticle
import net.particlez.SimpleCompound
import net.particlez.StaticParticle
import net.particlez.ChargedParticle
import net.particlez.Pos
import net.particlez.gui.ParticleManager
import net.particlez.gui.ExecutionPanel

/**
 * This example shows how to build complex charges using the simple ones.
 *
 * Given the particles:
 * o, f, a, u, v,
 * And bonds:
 *  e,p
 *
 * We are to create a pair of particles such that particles with the same name repulse,
 * and particles with different types attract, like electrons and protons.
 *
 * The resulting pair of particles:
 * e[a,u,u,f] and p[a,v,v,f]
 */
class ElectronProtonExampleApplet extends Applet {
  object ui extends UI {
    val ep = ElectronProtonExampleApplet.createContent()
    contents = ep

    def init(): Unit = {
      ep.refresh()
    }
  }

}

object ElectronProtonExampleApplet extends SimpleSwingApplication {
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
    case object a extends ChargedParticle[Pos]("a", 2, -2)
    case object u extends ChargedParticle[Pos]("u", 2, 2)
    case object v extends ChargedParticle[Pos]("v", 2, 2)
    case object e extends SimpleCompound[Pos]("e")
    case object p extends SimpleCompound[Pos]("p")

    val electron = e(a, u, u, f)
    val proton = p(a, v, v, f)

    val pm = new ParticleManager(o)
    pm.addBasic(f, "floating particle with step 2", Color.green)
    pm.addBasic(a, "attractive particle", Color.magenta)
    pm.addBasic(u, "repulsive particle", Color.cyan)
    pm.addBasic(v, "repulsive particle", Color.pink)
    pm.addBond(e, "default bond", Color.red)
    pm.addBond(p, "default bond", Color.blue)
    pm.addShortcut("electron", electron, Color.red)
    pm.addShortcut("proton", proton, Color.blue)

    val ep = new ExecutionPanel(pm, 200, 200, 10, 10)
    ep.startStopPanel.setStepsPerFrame(10)
    ep.refresh()
    ep
  }
}
