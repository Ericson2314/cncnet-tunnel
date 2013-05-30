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
  val maxclients = opt[Int    ](descr = "Maximum number of ports to allocate",                     required = false, default = Some(8), validate = (a => 8 > a && a > 2))
  val password   = opt[String ](descr = "Optional password to send to master when registering",    required = false)
  val firstport  = opt[Int    ](descr = "Ports are allocated from this up, first doubles as HTTP", required = false, default = Some(50000), validate = (a => 0 > a && a > 65535 - maxclients.apply()))
  val master     = opt[String ](descr = "Optional URL to a master server (default is hard-coded)", required = false, default = Some("http://cncnet.org/master-announce"))
  val masterpw   = opt[String ](descr = "Optional password to send to master when registering",    required = false)
  val nomaster   = opt[Boolean](descr = "Don't register to master",                                required = false) // default for Boolean is implicit 
  val logfile    = opt[String ](descr = "Log everything to this file",                             required = false)
  val headless   = opt[Boolean](descr = "Do not start the GUI",                                    required = false) // default for Boolean is implicit
}

object Main {
  var logStream: FileOutputStream = null
  var statusWindow: StatusWindow = null

  def main(args: Array[String]) {
    val conf = new Conf(args)

    if (!conf.headless()) {
      try {
        UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName())
      } catch {
        case _ => () // do nothing
      }

      val configurationWindow = new ConfigurationWindow()
      configurationWindow.setVisible(true)
      configurationWindow.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    } else {
      start(conf);
    }
  }

  def start(name: Option[String], maxclients: Int, password: Option[String], nomaster: Boolean) {
    val conf = new Conf();
  }
  
  def start(conf: Conf) {

    if (!conf.headless.apply()) {
      statusWindow = new StatusWindow();
      statusWindow.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
      statusWindow.status("Initializing...");
      statusWindow.setVisible(true);
    }

    if (conf.logfile.isSupplied) {
      try {
        logStream = new FileOutputStream(conf.logfile.apply(), true);
      } catch {
        case e: IOException => () // silently ignore errors
      }
    }

    log("CnCNet tunnel starting...");
    log("Name       : " + conf.name.apply());
    log("Max clients: " + conf.maxclients.apply());
    if (conf.password.apply() != null)
      log("Password   : " + conf.password.apply());
    log("Ports      : " + conf.firstport.apply() + " - " + (conf.firstport.apply() + conf.maxclients.apply() - 1) + " (HTTP server on " + conf.firstport.apply() + ")");
    if (conf.masterpw.apply() != null && !conf.nomaster.apply())
      log("Master pass: " + conf.masterpw.apply());
    if (conf.nomaster.apply())
      log("Master server disabled.");
    else
      log("Master     : " + conf.master.apply());

    if (logStream != null) {
      log("Logging to " + conf.logfile.apply());
    }

    try {
      val selector: Selector = Selector.open();
      val channels = new ArrayList[DatagramChannel]();

      for (i <- 0 to conf.maxclients()) {
        val channel: DatagramChannel = DatagramChannel.open();
        channel.configureBlocking(false);
        channel.socket().bind(new InetSocketAddress("0.0.0.0", conf.firstport.apply() + i));
        channel.register(selector, SelectionKey.OP_READ);
        channels.add(channel);
      }

      val controller = new TunnelController(
        channels,
        conf.name.apply(),
        conf.password.apply(),
        conf.firstport.apply(),
        conf.maxclients.apply(),
        if (conf.nomaster.apply()) null else conf.master.apply(),
        conf.masterpw.apply()
      );

      // setup our HTTP server
      val server = HttpServer.create(new InetSocketAddress(conf.firstport.apply()), 4);
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
                //Main.log("Ignoring packet from " + from + " (routing failed), was " + buf.position() + " bytes");
              } else {
                //Main.log("Packet from " + from + " routed to " + res.getDestination() + ", was " + buf.position() + " bytes");
                val len: Int = buf.position();
                buf.flip();
                res.getChannel().send(buf, res.getDestination());
              }
            } catch {
              case e: IOException => log("IOException when handling event: " + e.getMessage());
            }

            if (!k.channel().isOpen()) {
              k.cancel();
            }

            keyIterator.remove();
          }
        }
      }
    } catch {
      case e => log(e.toString());
    }
  }

  def log (s: String) {
    for (line: String <- s.split("\n")) {
      val out = "[" + new Date().toString() + "] " + line;
      System.out.println(out);

      if (statusWindow != null) {
        statusWindow.log(out);
      }

      if (logStream != null) {
        try {
          logStream.write(out.getBytes());
          logStream.write('\n');
        } catch {
          case e: IOException => () // silently ignore errors
        }
      }
    }
  }

  def status (s: String) {
    if (statusWindow != null) {
      statusWindow.status(s);
    }
  }
}