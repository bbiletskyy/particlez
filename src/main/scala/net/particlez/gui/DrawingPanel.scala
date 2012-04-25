package net.particlez.gui
import java.awt.image.BufferedImage
import java.awt.Color
import java.awt.Dimension
import java.awt.Graphics2D
import scala.swing.event.MouseClicked
import scala.swing.Panel
import net.particlez.Pos
import net.particlez.PosConfiguration
import scala.swing.event.Event

class DrawingPanel(val pm: ParticleManager, val w: Int = 200, val h: Int = 200) extends Panel {
  preferredSize = new Dimension(w, h)
  opaque = true
  var repaints = 0
  val bufferedImage = new BufferedImage(w, h, BufferedImage.TYPE_INT_BGR)
  var cellSizeX: Int = 0
  var cellSizeY: Int = 0
  tooltip = "Current configuration"
  listenTo(mouse.clicks)
  reactions += {
    case e: MouseClicked => publish(LocationSelected(e.point.x / cellSizeX, e.point.y / cellSizeY))
  }

  override def paintComponent(g: Graphics2D) {
    super.paintComponent(g)
    g.drawImage(bufferedImage, null, 0, 0)
    //println("repaints#" + repaints)
    repaints = repaints + 1
  }

  def drawConfiguration(c: PosConfiguration) = {
    val g = bufferedImage.createGraphics();
    assert(c.limits.size == 2)
    g.setColor(Color.white)
    g.fillRect(0, 0, h, w)
    cellSizeX = (w / c.limits(0)).toInt
    cellSizeY = (h / c.limits(1)).toInt
    for (x <- 0 until c.limits(0).toInt; y <- 0 until c.limits(1).toInt) {
      val particle = c.content(Pos(x, y))
      pm.particleIcon(particle, cellSizeX, cellSizeY).paintIcon(null, g, cellSizeX * x, cellSizeY * y)
    }
    super.repaint()
  }
}

case class LocationSelected(x: Int, y: Int) extends Event
