package org.cncnet.tunnel

import java.nio.channels.DatagramChannel
import java.nio.channels.SelectionKey
import java.nio.channels.Selector
import java.nio.ByteBuffer
import java.net.InetSocketAddress
import java.io.IOException
import java.util.concurrent.BlockingQueue
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.ArrayBlockingQueue

import scala.collection.mutable.Map
import scala.collection.JavaConversions._

class Dispatcher(logger: Logger, selector: Selector) extends Runnable {

  private val routers: Map[DatagramChannel, Router] = new ConcurrentHashMap[DatagramChannel, Router]()

  private def requestRoute(source: InetSocketAddress, destination: DatagramChannel, now: Long): Option[(InetSocketAddress, DatagramChannel)] = {
    routers.get(destination) match {
      case Some(router) => router.route(source, destination, now)
      case None => None
    }
  }

  override def run() {
    try actualRun() catch { case e => logger.log(e.getLocalizedMessage()) }
  }

  def actualRun() {
    val buf: ByteBuffer = ByteBuffer.allocate(4096);

    while (true) {
      if (selector.select() > 0) {
        val now: Long = System.currentTimeMillis();
        val keyIterator = selector.selectedKeys().iterator();

        while (keyIterator.hasNext()) {
          val key: SelectionKey = keyIterator.next()

          val destination: DatagramChannel = key.channel() match {
            case chan: DatagramChannel => chan
            case _                     => throw new ClassCastException
          }

          try {
            buf.clear();

            val source = destination.receive(buf) match {
              case source: InetSocketAddress => source
              case _                         => throw new ClassCastException
            }

            requestRoute(source, destination, now) match {
              case Some((destination: InetSocketAddress, channel: DatagramChannel)) => {
                //Main.logger.log("Packet from " + from + " routed to " + res.getDestination() + ", was " + buf.position() + " bytes");
                val len: Int = buf.position();
                buf.flip();
                channel.send(buf, destination);
              }
              case None => () //logger.log("Ignoring packet from " + from + " (routing failed), was " + buf.position() + " bytes")
            }
          } catch {
            case e: IOException => logger.log("IOException when handling event: " + e.getLocalizedMessage());
          }

          if (!key.channel().isOpen()) {
            key.cancel();
          }

          keyIterator.remove();
        }
      }
    }
  }

  // for TunnelController

  def numRouters = routers.size
  def routerKVs = routers.iterator
  def addRouter(k: DatagramChannel, v: Router): Unit = routers.put(k, v)
}
