package net.particlez.gui
import java.awt.Color
import java.awt.Dimension
import scala.swing.Table.LabelRenderer
import scala.swing.event.WindowClosing
import scala.swing.Alignment
import scala.swing.Component
import scala.swing.Frame
import scala.swing.ScrollPane
import scala.swing.SimpleSwingApplication
import scala.swing.Table
import javax.swing.table.DefaultTableModel
import javax.swing.UIManager
import net.particlez.ChargedParticle
import net.particlez.FloatingParticle
import net.particlez.SimpleCompound
import net.particlez.StaticParticle
import net.particlez.Pos
import javax.swing.table.AbstractTableModel

class ParticleManagerPanel(val items: Set[ParticleItem]) extends ScrollPane {
  preferredSize = new Dimension(300, 100)
  val particleTable = new ParticleTable(items)
  contents = particleTable
}

class ParticleTable(items: Set[ParticleItem]) extends Table {
  model = new AbstractTableModel {
    val columnNames: Seq[_] = Seq("", "Name", "Description")
    val rowData: Array[Array[Any]] = items.map(Array[Any](_, null, null)).toArray
    override def getColumnName(column: Int) = columnNames(column).toString
    def getRowCount() = rowData.length
    def getColumnCount() = columnNames.length
    def getValueAt(row: Int, col: Int): AnyRef = rowData(row)(col).asInstanceOf[AnyRef]
    override def isCellEditable(row: Int, column: Int) = false
    override def setValueAt(value: Any, row: Int, col: Int) {
      rowData(row)(col) = value
      fireTableCellUpdated(row, col)
    }
  }
  //autoResizeMode = Table.AutoResizeMode.Off
  peer.getColumnModel().getColumn(0).setPreferredWidth(10)
  peer.getColumnModel().getColumn(1).setPreferredWidth(50)
  peer.getColumnModel().getColumn(2).setPreferredWidth(220)

  val iconRenderer = new LabelRenderer[ParticleItem]((e: ParticleItem) => (e.icon(e.particle, 12), ""))
  val nameRenderer = new LabelRenderer[ParticleItem]((e: ParticleItem) => (null, e.name))
  val descriptionRenderer = new LabelRenderer[ParticleItem]((e: ParticleItem) => (null, e.description)) {component.xAlignment = Alignment.Left}
  
  override def rendererComponent(isSelected: Boolean, hasFocus: Boolean, row: Int, col: Int): Component = {
    val v = model.getValueAt(peer.convertRowIndexToModel(row), peer.convertColumnIndexToModel(0)).asInstanceOf[ParticleItem]
    col match {
      case 0 => iconRenderer.componentFor(this, isSelected, hasFocus, v, row, col)
      case 1 => nameRenderer(v.name).componentFor(this, isSelected, hasFocus, v, row, col)
      case 2 => descriptionRenderer(v.description).componentFor(this, isSelected, hasFocus, v, row, col)
    }
  }

  private def nameRenderer(tooltip: String): LabelRenderer[ParticleItem] = {
    //val nameRenderer = new LabelRenderer[ParticleItem]((e: ParticleItem) => (null, e.name))
    nameRenderer.component.tooltip = tooltip
    nameRenderer
  }

  private def descriptionRenderer(tooltip: String): LabelRenderer[ParticleItem] = {
    //val descriptionRenderer = new LabelRenderer[ParticleItem]((e: ParticleItem) => (null, e.description))
    //descriptionRenderer.component.xAlignment = Alignment.Left
    descriptionRenderer.component.tooltip = tooltip
    descriptionRenderer
  }
}

object ParticleManagerPanel extends SimpleSwingApplication {
  val frame = new Frame {
    contents = createContent()
    reactions += {
      case WindowClosing(_) => System.exit(0)
    }
  }
  def top = frame

  private def createContent(): Component = {
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
    new ParticleManagerPanel(pm.items())
  }
}

