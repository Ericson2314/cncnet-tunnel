package org.cncnet.tunnel

import java.nio.channels.DatagramChannel
import java.nio.channels.SelectionKey
import java.nio.channels.Selector
import java.nio.ByteBuffer
import java.net.InetSocketAddress
import java.io.IOException

class Dispatcher(
  selector: Selector,
  controller: TunnelController
) extends Runnable {

  override def run() {
    val buf: ByteBuffer = ByteBuffer.allocate(4096);

    while (true) {
      if (selector.select() > 0) {
        val now: Long = System.currentTimeMillis();
        val keyIterator = selector.selectedKeys().iterator();

        while (keyIterator.hasNext()) {
          val key: SelectionKey = keyIterator.next()

          val destination: DatagramChannel = key.channel() match {
            case chan: DatagramChannel => chan
            case _ => throw new ClassCastException
          }

          try {
            buf.clear();

            val source = destination.receive(buf) match {
              case source: InetSocketAddress => source
              case _ => throw new ClassCastException
            }

            controller.requestRoute(source, destination, now) match {
              case Some((destination: InetSocketAddress, channel: DatagramChannel)) => {
                //Main.logger.log("Packet from " + from + " routed to " + res.getDestination() + ", was " + buf.position() + " bytes");
                val len: Int = buf.position();
                buf.flip();
                channel.send(buf, destination);
              }
              case None => () //logger.log("Ignoring packet from " + from + " (routing failed), was " + buf.position() + " bytes")
            }
          } catch {
            case e: IOException => new Exception("IOException when handling event: " + e.getMessage());
          }

          if (!key.channel().isOpen()) {
            key.cancel();
          }

          keyIterator.remove();
        }
      }
    }
  }
}