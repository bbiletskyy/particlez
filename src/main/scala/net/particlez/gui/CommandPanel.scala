package net.particlez.gui
import java.awt.Color
import scala.swing.event.ButtonClicked
import scala.swing.event.Event
import scala.swing.event.Key
import scala.swing.event.KeyPressed
import scala.swing.event.WindowClosing
import scala.swing.Button
import scala.swing.Dialog
import scala.swing.FlowPanel
import scala.swing.Frame
import scala.swing.Panel
import scala.swing.SimpleSwingApplication
import scala.swing.TextField
import javax.swing.UIManager
import net.particlez.FloatingParticle
import net.particlez.SimpleCompound
import net.particlez.Compound
import net.particlez.StaticParticle
import net.particlez.Particle
import net.particlez.Pos
import net.particlez.ParticleParser
import net.particlez.ParticleParserException
import scala.swing.BoxPanel
import scala.swing.Orientation
import net.particlez.ChargedParticle

class CommandPanel(val pm: ParticleManager) extends BoxPanel(Orientation.Vertical) {
  val particleTextField = new TextField(15) { tooltip = "Enter a particle then click on configuration to add it" }
  val addParticleButton = new Button("Add") { tooltip = "Add particle at the randomly chosen empty position" }
  val fillButton = new Button("Fill") { tooltip = "Fill every position with the particle" }

  contents += new FlowPanel(particleTextField, addParticleButton, fillButton)
  listenTo(addParticleButton, fillButton, particleTextField.keys)
  reactions += {
    case KeyPressed(`particleTextField`, Key.Enter, _, _) => publish(AddParticleEvent)
    case ButtonClicked(`addParticleButton`) => publish(AddParticleEvent)
    case ButtonClicked(`fillButton`) => publish(FillParticleEvent)
  }

  def getParticle(): Particle[Pos] = {
    val basics: Map[String, Particle[Pos]] = pm.basics().map(i => (i.name -> i.particle)).toMap
    val bonds: Map[String, Compound[Pos]] = pm.bonds().map(i => (i.particle.name -> i.particle.asInstanceOf[Compound[Pos]])).toMap
    new ParticleParser(basics, bonds)(particleTextField.text.trim())
  }
}

case object AddParticleEvent extends Event
case object FillParticleEvent extends Event

object CommandPanel extends SimpleSwingApplication {
  val frame = new Frame {
    contents = createContent()
    reactions += {
      case WindowClosing(_) => System.exit(0)
    }
  }
  def top = frame

  private def createContent(): Panel = {
    case object o extends StaticParticle[Pos]("o")
    case object f extends FloatingParticle[Pos]("f", 5, o)
    case object e extends ChargedParticle[Pos]("e", 5, -5)
    case object b extends SimpleCompound[Pos]("b")
    case object d extends SimpleCompound[Pos]("d")

    val pm = new ParticleManager(o)
    pm.addBasic(f, "floating particle", Color.blue)
    pm.addBasic(e, "charged particle", Color.red)
    pm.addBond(b, "Simple bond", Color.green)
    pm.addBond(d, "Another bond called d", Color.orange)
    UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName);
    new CommandPanel(pm)
  }
}
