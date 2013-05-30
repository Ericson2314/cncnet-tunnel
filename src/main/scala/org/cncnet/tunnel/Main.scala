package org.cncnet.tunnel

import org.rogach.scallop._;

import com.sun.net.httpserver.HttpServer;
import java.io.FileOutputStream;
import java.io.IOException;
import java.net.InetSocketAddress;
import java.nio.ByteBuffer;
import java.nio.channels.DatagramChannel;
import java.nio.channels.SelectionKey;
import java.nio.channels.Selector;
import java.util.ArrayList;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import javax.swing.JFrame;
import javax.swing.UIManager;

class Conf(arguments: Seq[String]) extends ScallopConf(arguments) {
  val name       = opt[String ](descr = "Custom name for the tunnel",                              required = false, default = Some("Unnamed CnCNet 5 tunnel"))
  val maxClients = opt[Int    ](descr = "Maximum number of ports to allocate",                     required = false, default = Some(8), validate = (a => 8 > a && a > 2))
  val password   = opt[String ](descr = "Optional password to send to master when registering",    required = false)
  val firstPort  = opt[Int    ](descr = "Ports are allocated from this up, first doubles as HTTP", required = false, default = Some(50000),
    validate = (a => 0 > a && a > Main.maxPort - maxClients.apply())
  )
  val master     = opt[String ](descr = "Optional URL to a master server (default is hard-coded)", required = false, default = Some("http://cncnet.org/master-announce"))
  val masterPW   = opt[String ](descr = "Optional password to send to master when registering",    required = false)
  val nomaster   = opt[Boolean](descr = "Don't register to master",                                required = false) // default for Boolean is implicit 
  val logfile    = opt[String ](descr = "Log everything to this file",                             required = false)
  val headless   = opt[Boolean](descr = "Do not start the GUI",                                    required = false) // default for Boolean is implicit
  
  // for java
  def getMaxClients(): Int = maxClients.apply;
}

object Main {
  val maxPort: Int =  65535
  
  
  def main(args: Array[String]) {
    val conf = new Conf(args)
    val logger = new Logger(conf.logfile.get, if (conf.headless.apply()) None else {
      val statusWindow = new StatusWindow();
      statusWindow.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
      statusWindow.status("Initializing...");
      statusWindow.setVisible(true);
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
      start(conf, logger);
    }
  }

  def start(conf: Conf, logger: Logger, name: Option[String], maxclients: Int, password: Option[String], nomaster: Boolean) {
    val conf = new Conf();
    start(conf, logger)
  }
  
  def start(conf: Conf, logger: Logger) {

    logger.log("CnCNet tunnel starting...");
    logger.log("Name       : " + conf.name.apply());
    logger.log("Max clients: " + conf.maxClients.apply());
    logger.log(conf.password.get match {
      case Some(pw) => "Password   : " + pw
      case None     => "***No Password***"
    })
    logger.log("Ports      : " + conf.firstPort.apply() + " - " + (conf.firstPort.apply() + conf.maxClients.apply() - 1) + " (HTTP server on " + conf.firstPort.apply() + ")");
    
    logger.log(conf.masterPW.get match {
      case Some(pw) => "Master pass: " + pw
      case None     => "***No Master Password***"
    })
    logger.log(if (conf.nomaster.apply()) "Master server disabled." else "Master     : " + conf.master.apply())
    logger.log(conf.logfile.get match {
      case Some(f) => "Logging to:   " + f
      case None     => "***No Log File***"
    })

    try {
      val selector: Selector = Selector.open();
      val channels = new ArrayList[DatagramChannel]();

      for (offset <- 0 to conf.maxClients()) {
        val channel: DatagramChannel = DatagramChannel.open();
        channel.configureBlocking(false);
        channel.socket().bind(new InetSocketAddress("0.0.0.0", conf.firstPort.apply() + offset));
        channel.register(selector, SelectionKey.OP_READ);
        channels.add(channel);
      }

      val controller = new TunnelController(
        logger,
        channels,
        conf.name.apply(),
        conf.password.apply(),
        conf.firstPort.apply(),
        conf.maxClients.apply(),
        if (conf.nomaster.apply()) null else conf.master.apply(),
        conf.masterPW.apply()
      );

      // setup our HTTP server
      val server = HttpServer.create(new InetSocketAddress(conf.firstPort.apply()), 4);
      server.createContext("/request", controller);
      server.createContext("/status", controller);
      server.setExecutor(null);
      server.start();

      new Thread(controller).start();

      val buf: ByteBuffer = ByteBuffer.allocate(4096);

      while (true) {
        if (selector.select() > 0) {
          val now: Long = System.currentTimeMillis();
          val keyIterator = selector.selectedKeys().iterator();
          
          while (keyIterator.hasNext()) {
            val k: SelectionKey = keyIterator.next()
            
            val chan: DatagramChannel = k.channel() match {
              case chan: DatagramChannel => chan
              case _                     => throw new ClassCastException
            }

            try {
              buf.clear();

              val from = chan.receive(buf) match {
                case chan: InetSocketAddress => chan
                case _                       => throw new ClassCastException
              }
              
              val router: Router = controller.getRouter(chan);
              val res: RouteResult = if (router == null) null else router.route(from, chan, now);
              if (res == null) {
                //Main.logger.log("Ignoring packet from " + from + " (routing failed), was " + buf.position() + " bytes");
              } else {
                //Main.logger.log("Packet from " + from + " routed to " + res.getDestination() + ", was " + buf.position() + " bytes");
                val len: Int = buf.position();
                buf.flip();
                res.getChannel().send(buf, res.getDestination());
              }
            } catch {
              case e: IOException => logger.log("IOException when handling event: " + e.getMessage());
            }

            if (!k.channel().isOpen()) {
              k.cancel();
            }

            keyIterator.remove();
          }
        }
      }
    } catch {
      case e => logger.log(e.toString());
    }
  }
}