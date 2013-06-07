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

  def log(str: String) {
    SwingUtilities.invokeLater(new Runnable() {
      override def run() {
        logArea.text.length() match {
          case 0 => logArea.append(str)
          case _ => logArea.append("\n" + str)
        }
        statusLabel.text = str
        logArea.caret.position = logArea.text.length()
      }
    })
  }

  def status(str: String) {
    SwingUtilities.invokeLater(new Runnable() {
      override def run() {
        statusLabel.text = str
        StatusWindow.this.title = "CnCNet Tunnel - " + str
      }
    })
  }

  // must be below where logPane and statusPanel are defined

  contents = new BoxPanel(Orientation.Vertical) {
    contents += logPane
    contents += statusPanel
  }
}