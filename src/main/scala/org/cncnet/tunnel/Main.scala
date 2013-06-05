package org.cncnet.tunnel

import org.rogach.scallop._
import com.sun.net.httpserver.HttpServer
import java.io.File
import java.io.FileOutputStream
import java.io.IOException
import java.net.InetSocketAddress
import java.nio.ByteBuffer
import java.nio.channels.DatagramChannel
import java.nio.channels.SelectionKey
import java.nio.channels.Selector
import javax.swing.JFrame
import javax.swing.UIManager

class Conf(arguments: Seq[String]) extends ScallopConf(arguments) {
  val name       = opt[String ](descr = "Custom name for the tunnel",                              required = false, default = Some("Unnamed CnCNet 5 tunnel"))
  val maxClients = opt[Int    ](descr = "Maximum number of ports to allocate",                     required = false, default = Some(8), validate = _>=2)
  val password   = opt[String ](descr = "Optional password to send to master when registering",    required = false)
  val firstPort  = opt[Int    ](descr = "Ports are allocated from this up, first doubles as HTTP", required = false, default = Some(50000),
    validate = (a => Main.maxPort - maxClients.apply() >= a && a >= 0)
  )
  val master     = opt[String ](descr = "Optional URL to a master server (default is hard-coded)", required = false, default = Some("http://cncnet.org/master-announce"))
  val masterPW   = opt[String ](descr = "Optional password to send to master when registering",    required = false)
  val nomaster   = opt[Boolean](descr = "Don't register to master",                                required = false) // default for Boolean is implicit 
  val logfile    = opt[String ](descr = "Log everything to this file",                             required = false)
  val headless   = opt[Boolean](descr = "Do not start the GUI",                                    required = false) // default for Boolean is implicit
  
  // for java
  def getMaxClients(): Int = maxClients.apply
}

object Main {
  val maxPort: Int =  65535
  
  
  def main(args: Array[String]) {
    val conf = new Conf(args)
    val logger = Logger(conf.logfile.get, if (conf.headless.apply()) None else {
      val statusWindow = new StatusWindow() {
        override def closeOperation() { System exit 0 } // exit on close
      }
      statusWindow.status("Initializing...")
      statusWindow.visible_=(true)
      Some(statusWindow)
    })

    if (!conf.headless()) {
      try {
        UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName())
      } catch {
        case _ => () // do nothing
      }

      val configurationWindow = new ConfigurationWindow(conf, logger)
      configurationWindow.setVisible(true)
      configurationWindow.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    } else {
      start(conf, logger)
    }
  }

  // bootstraps on def below this
  def start(conf: Conf, logger: Logger): Unit = start(
    conf,
    logger,
    conf.name.apply,
    conf.maxClients.apply,
    conf.password.get,
    conf.nomaster.apply
  )

  def start(conf: Conf, logger: Logger,
    name: String, maxclients: Int, password: Option[String], nomaster: Boolean
  ): Unit = start(
    name,
    maxclients,
    password,
    conf.firstPort.apply,
    if (conf.nomaster.apply) None else conf.master.get, // latter is always present because default server is hard-coded
    conf.masterPW.get,
    conf.logfile.get.map(s => new File(s)),
    logger
  )
  
  def start(
    name: String,
    maxClients: Int,
    password: Option[String],
    firstPort: Int,
    master: Option[String],
    masterPW: Option[String],
    logfile: Option[File],
    logger: Logger
  ) {

    logger.log("CnCNet tunnel starting...")
    logger.log("Name       : " + name)
    logger.log("Max clients: " + maxClients)
    logger.log(password match {
      case Some(pw) => "Password   : " + pw
      case None     => "***No Password***"
    })
    logger.log("Ports      : " + firstPort + " - " + (firstPort + maxClients - 1) + " (HTTP server on " + firstPort + ")")
    
    logger.log(masterPW match {
      case Some(pw) => "Master pass: " + pw
      case None     => "***No Master Password***"
    })
    logger.log(master match {
      case Some(mr) => "Master     : " + mr
      case None     => "***No Master Server***"
    })
    logger.log(logfile match {
      case Some(f) => "Logging to:   " + f
      case None    => "***No Log File***"
    })

    try {
      val selector: Selector = Selector.open()

      def createChannel (portNum: Int): DatagramChannel = {
        val channel: DatagramChannel = DatagramChannel.open()
        channel.configureBlocking(false)
        channel.socket().bind(new InetSocketAddress(portNum)) // tunnel port
        channel.register(selector, SelectionKey.OP_READ)
        channel
      }

      val dispatcher = new Dispatcher(logger, selector)

      val controller = new TunnelController(
        logger,
        dispatcher,
        Array.range(0, maxClients).map(firstPort + _).map(createChannel),
        name,
        password,
        firstPort,
        maxClients,
        master,
        master)

      // setup our HTTP server
      val server = HttpServer.create(new InetSocketAddress(firstPort), 4)
      server.createContext("/request", controller)
      server.createContext("/status", controller)
      server.setExecutor(null)
      server.start()

      new Thread(dispatcher).start()
      new Thread(controller).start()
    } catch {
      case e => logger.log(e.toString())
    }
  }
}
