package net.particlez.gui
import java.awt.BasicStroke
import java.awt.Color
import java.awt.Graphics
import java.awt.Graphics2D
import scala.collection.mutable.HashMap
import javax.swing.Icon
import net.particlez.ChargedParticle
import net.particlez.Compound
import net.particlez.FloatingParticle
import net.particlez.SimpleCompound
import net.particlez.StaticParticle
import net.particlez.Particle
import net.particlez.Pos
import scala.util.Random

/** Holds the particles, shortcuts and icons  */
class ParticleManager(val o: Particle[Pos]) {
  val namedItems = new HashMap[String, ParticleItem]()
  addBasic(o, "empty space placeholder", Color.white)
  def empty = o
  val unknownParticleIcon = new UnknownIcon(Color.red, null)

  def items(): Set[ParticleItem] = namedItems.values.toSet
  def basics(): Set[ParticleItem] = items().filter(e => !e.isBond).toSet
  def bonds(): Set[ParticleItem] = items().filter(e => e.isBond).toSet
  def basicParticles(): Set[Particle[Pos]] = basics.map(_.particle)
  def bondParticles(): Set[Compound[Pos]] = bonds.map(_.particle.asInstanceOf[Compound[Pos]])

  def addBasic(b: Particle[Pos], d: String, c: Color = Color.gray) {
    namedItems += (b.name() -> ParticleItem(b.name, b, d, new BasicIcon(c, b), false))
  }

  def addShortcut(n: String, p: Particle[Pos], c: Color = Color.gray) {
    namedItems += (n -> ParticleItem(n, p, "shortcut for " + p.toString(), new BasicIcon(c, p), false))
  }

  def addBond(b: Compound[Pos], d: String, c: Color = Color.gray) {
    namedItems += (b.name() -> ParticleItem(b.name + "[.]", b, d, new BondIcon(c, b), true))
  }

  //  def particleIcon(p: Particle[Pos], w: Int, h: Int): ParticleIcon = namedItems.get(p.name()) match {
  //    case Some(particleItem) => particleItem.icon(p, w, h)
  //    case None => unknownParticleIcon(p, w, h)
  //  }

  def particleIcon(p: Particle[Pos], w: Int, h: Int): ParticleIcon = {
    namedItems.find(e => e._2.particle == p) match {
      case Some(e) => e._2.icon(p, w, h)
      case None => namedItems.get(p.name()) match {
        case Some(i) => i.icon(p, w, h)
        case None => unknownParticleIcon(p, w, h)
      }
    }
  }
}

abstract class ParticleIcon(val p: Particle[Pos], val w: Int, val h: Int) extends Icon {
  def this(particle: Particle[Pos], size: Int) = this(particle, size, size)
  def paintIcon(comp: java.awt.Component, g: Graphics, x: Int, y: Int)
  def getIconWidth() = w
  def getIconHeight() = h
  def apply(particle: Particle[Pos], width: Int, height: Int): ParticleIcon
  def apply(particle: Particle[Pos], size: Int): ParticleIcon = apply(particle, size, size)
}

class BasicIcon(val c: Color, p: Particle[Pos], w: Int = 0, h: Int = 0) extends ParticleIcon(p, w, h) {
  override def paintIcon(comp: java.awt.Component, g: Graphics, x: Int, y: Int) = {
    g.setColor(c)
    g.fillOval(x + (w / 8), y + (h / 8), 3 * w / 4, 3 * h / 4)
  }
  override def apply(particle: Particle[Pos], width: Int, height: Int): ParticleIcon = new BasicIcon(c, particle, width, height)
}

class UnknownIcon(val c: Color, p: Particle[Pos], w: Int = 0, h: Int = 0) extends ParticleIcon(p, w, h) {
  override def paintIcon(comp: java.awt.Component, g: Graphics, x: Int, y: Int) = {
    g.setColor(c)
    g.fillRect(x + (w / 8), y + (h / 8), 3 * w / 4, 3 * h / 4)
  }
  override def apply(particle: Particle[Pos], width: Int, height: Int): ParticleIcon = new UnknownIcon(c, particle, width, height)
}

class BondIcon(val c: Color, p: Particle[Pos], w: Int = 0, h: Int = 0) extends ParticleIcon(p, w, h) {
  override def paintIcon(comp: java.awt.Component, g0: Graphics, x: Int, y: Int) = {
    val g = g0.asInstanceOf[Graphics2D]
    g.setColor(Color.darkGray)
    val nestedCount = p.asInstanceOf[Compound[Pos]].nested().size
    for (i <- 0 until nestedCount) {
      val nx = x + (w / 8) + Random.nextInt(w / 2)
      val ny = y + (h / 8) + Random.nextInt(h / 2)
      g.fillOval(nx, ny, w / 4, h / 4)
    }
    g.setColor(c)
    g.setStroke(new BasicStroke(2))
    g.drawOval(x + 1, y + 1, w - 2, h - 2)
  }
  override def apply(particle: Particle[Pos], width: Int, height: Int): ParticleIcon = {
    new BondIcon(c, particle, width, height)
  }
}

case class ParticleItem(name: String, particle: Particle[Pos], description: String, icon: ParticleIcon, isBond: Boolean)

object ParticleManager {
  def main(args: Array[String]) {
    case object o extends StaticParticle[Pos]("o")
    case object f extends FloatingParticle[Pos]("f", 5, o)
    case object e extends ChargedParticle[Pos]("e", 5, -5)
    case object b extends SimpleCompound[Pos]("b")
    case object d extends SimpleCompound[Pos]("d")

    val pm = new ParticleManager(o)
    //pm.addBasic(o, "empty particle", Color.white)
    pm.addBasic(f, "floating particle", Color.blue)
    pm.addBasic(e, "charged particle", Color.red)
    println(pm.empty)
  }
}