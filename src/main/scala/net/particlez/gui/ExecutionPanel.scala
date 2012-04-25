package net.particlez.gui
import java.awt.Color
import scala.annotation.elidable
import scala.swing.event.ButtonClicked
import scala.swing.event.Event
import scala.swing.event.WindowClosing
import scala.swing.BorderPanel
import scala.swing.BoxPanel
import scala.swing.Button
import scala.swing.Component
import scala.swing.FlowPanel
import scala.swing.Frame
import scala.swing.Label
import scala.swing.Orientation
import scala.swing.SimpleSwingApplication
import scala.swing.Swing
import scala.util.Random
import annotation.elidable.ASSERTION
import javax.swing.JSpinner
import javax.swing.SpinnerNumberModel
import javax.swing.UIManager
import net.particlez.BlockCompound
import net.particlez.ChargedParticle
import net.particlez.FloatingParticle
import net.particlez.SimpleCompound
import net.particlez.StaticParticle
import net.particlez.BindParticle
import net.particlez.Particle
import net.particlez.UpParticle
import net.particlez.DownParticle
import net.particlez.Pos
import net.particlez.PosConfiguration
import net.particlez.UnBindParticle
import net.particlez.ParticleParserException
import scala.swing.Dialog

class ExecutionPanel(val pm: ParticleManager, val dw: Int = 300, val dh: Int = 300, val cw: Int = 20, val ch: Int = 20) extends BorderPanel {
  border = Swing.EmptyBorder(5, 5, 5, 5)
  val drawingPanel = new DrawingPanel(pm, dw, dh)
  val particleManagerPanel = new ParticleManagerPanel(pm.items())
  val commandPanel = new CommandPanel(pm)
  val startStopPanel = new StartStopPanel
  val configuration = new PosConfiguration(pm.empty, cw, ch)

  val controlsPanel = new BoxPanel(Orientation.Vertical) {
    contents ++= Seq(particleManagerPanel, commandPanel, startStopPanel)
    border = Swing.EmptyBorder(0, 5, 0, 0)
  }
  add(drawingPanel, BorderPanel.Position.Center)
  add(controlsPanel, BorderPanel.Position.East)

  listenTo(startStopPanel, commandPanel, drawingPanel)
  reactions += {
    case TransformAndRefresh(times) =>
      transform(times)
      refresh()
    case AddParticleEvent =>
      try {
        addParticleRandomly(commandPanel.getParticle())
        refresh()
      } catch {
        case e: RuntimeException => handleException(e)
      }
    case FillParticleEvent =>
      try {
        fillWithParticles(commandPanel.getParticle())
        refresh()
      } catch {
        case e: ParticleParserException => handleException(e)
      }
    case LocationSelected(x, y) =>
      try {
        val p = commandPanel.getParticle()
        addParticleAt(p, Pos(x, y))
        refresh()
      } catch {
        case e: ParticleParserException => handleException(e)
      }
  }

  private def handleException(e: RuntimeException) {
    Dialog.showMessage(this, e.getMessage())
  }

  def addMembraneOf(p: Particle[Pos]) = configuration.synchronized {
    val mx = configuration.limits(0) / 2
    val newItems = configuration.locations().filter(l => l.coordinates(0) == mx.toInt).map(l => (l -> p.transform(pp => pp))).toMap
    configuration.content ++= newItems
  }

  def fillWithParticles(p: Particle[Pos]) = configuration.synchronized {
    configuration.fill(p)
  }

  def addParticleRandomly(p: Particle[Pos]) = configuration.synchronized {
    val emptyLocations = configuration.emptyLocations()
    if (emptyLocations.isEmpty) throw new RuntimeException("There are no more empty locations left to add a particle")
    val l = emptyLocations(Random.nextInt(emptyLocations.size))
    assert(configuration.content(l) == pm.empty)
    addParticleAt(p, l)
  }

  def addParticleAt(p: Particle[Pos], l: Pos) = configuration.synchronized {
    configuration.content += (l -> p)
  }

  private def transform(times: Int) = configuration.synchronized {
    for (i <- 0 until times) configuration.update()
  }

  def refresh() = configuration.synchronized {
    drawingPanel.drawConfiguration(configuration)
  }

}

case class TransformAndRefresh(updateTimes: Int) extends Event

class StartStopPanel extends FlowPanel {
  val startStopButton = new Button("Start") { tooltip = "Start/Stop simulation" }

  val stepsPerFrameSpinner = new JSpinner(new SpinnerNumberModel(10, 1, 1000, 1))
  stepsPerFrameSpinner.setToolTipText("The number of particle interactions per frame (only active particle are taken into account)")
  var running = false

  contents += startStopButton
  contents += new Label("Interactions per frame: ")
  contents + Component.wrap(stepsPerFrameSpinner)

  listenTo(startStopButton)
  reactions += {
    case ButtonClicked(startStopButton) => startStop()
  }

  def getStepsPerFrame = stepsPerFrameSpinner.getModel().getValue().asInstanceOf[Int]
  def setStepsPerFrame(n: Int) = stepsPerFrameSpinner.getModel().setValue(n)

  def startStop() {
    running = !running
    if (running) {
      startStopButton.text = "Stop"
      new Thread() {
        override def run() = {
          while (running) {
            publish(TransformAndRefresh(getStepsPerFrame))
            Thread.sleep(100)
          }
        }
      }.start()
    } else {
      startStopButton.text = "Start"
    }
  }
}

object ExecutionPanel extends SimpleSwingApplication {
  val frame = new Frame {
    UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName())
    contents = createContent()
    reactions += {
      case WindowClosing(_) => System.exit(0)
    }
  }
  def createContent() = {
    UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName())
    case object o extends StaticParticle[Pos]("o")
    case object m extends StaticParticle[Pos]("m")
    case object f extends FloatingParticle[Pos]("f", 1, o)
    case object bb extends SimpleCompound[Pos]("bb")
    case object bep extends ChargedParticle[Pos]("bep", 0, +0.5)
    case object ben extends ChargedParticle[Pos]("ben", 0, -0.5)
    case object b extends BlockCompound[Pos]("b", p => if (p.flatten().exists(m == _)) bb(p, ben) else bb(p, bep))
    case object bc extends BindParticle[Pos]("bc", 3, b(), o, true)
    case object bd extends UnBindParticle[Pos]("bc", 3, b(), o, true)
    case object x extends SimpleCompound[Pos]("x")
    case object y extends SimpleCompound[Pos]("y")
    case object z extends SimpleCompound[Pos]("z")

    
    
    val pm = new ParticleManager(o)
    pm.addBasic(f, "floating particle, r=1")
    pm.addBond(x(), "x bond", Color.red)
    pm.addBond(y(), "y bond", Color.green)
    pm.addBond(z(), "z bond", Color.blue)
    pm.addBond(b(), "b bond", Color.orange)
    val ep = new ExecutionPanel(pm, 200, 200, 10, 10)

    val xx = x(f, bc, bd, m)
    println(b(xx).flatten())
    ep.addParticleRandomly(xx)
    ep.addParticleRandomly(xx)
    ep.addParticleRandomly(y(f))
    ep.addParticleRandomly(z(f, m))
    ep.startStopPanel.setStepsPerFrame(100)

    ep.refresh()
    ep
  }
  def top = frame
}





