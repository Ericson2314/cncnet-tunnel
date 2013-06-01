package org.cncnet.tunnel

import swing._
import java.awt.BorderLayout
import java.awt.Component._
import java.awt.Font
import javax.swing.ImageIcon
import javax.swing.BoxLayout
import javax.swing.border.EmptyBorder
import javax.swing.BorderFactory
import javax.swing.SwingUtilities

class StatusWindow extends MainFrame {
  title = "CnCNet Tunnel"
  iconImage = new ImageIcon("res/cncnet-icon.png").getImage()
  size = new Dimension(600, 600)
  contents = new BoxPanel(Orientation.Vertical) {
    contents += logPane
    contents += statusPanel
  }

  val logArea = new TextArea() {
    font = new Font("Monospaced", Font.PLAIN, 12)
    border = new EmptyBorder(2, 2, 2, 2)
    editable = true
  }

  val logPane = new ScrollPane(logArea) {
    xLayoutAlignment = LEFT_ALIGNMENT
  }

  val statusLabel = new Label(" ") {
    font = new Font("SansSerif", Font.PLAIN, 12)
    border = new EmptyBorder(2, 2, 2, 2)
  }
  
  val statusPanel = new BorderPanel {
    border = BorderFactory.createLoweredBevelBorder()
    xLayoutAlignment = LEFT_ALIGNMENT
    maximumSize = new Dimension(Integer.MAX_VALUE, 10)

    add(statusLabel, BorderPanel.Position.Center)
  }

  // used to convert a Unit => Unit function into a Java Runnable
  // got it off StackOverflow
  implicit def whateverToRunnable[F](f: => F) = new Runnable() { def run() { f } }

  def log(str: String) {
    SwingUtilities.invokeLater(() => {
      logArea.text.length() match {
        case 0 => logArea.append(str)
        case _ => logArea.append("\n" + str)
      }
      statusLabel.text_=(str)
      logArea.caret.position_=(logArea.text.length())
    })
  }

  def status(str: String) {
    SwingUtilities.invokeLater(() => {
      statusLabel.text_=(str)
      StatusWindow.this.title_=("CnCNet Tunnel - " + str)
    })
  }
}