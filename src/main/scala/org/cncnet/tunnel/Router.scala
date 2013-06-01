package org.cncnet.tunnel

import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.nio.channels.DatagramChannel;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import scala.collection.JavaConversions._

/**
 *
 * @author Toni Spets <toni.spets@iki.fi>
 */
object Router {
  def apply(ipMap: Map[InetAddress, DatagramChannel]): Router = {
    val portMap = new HashMap[DatagramChannel, Port]()

    for (entry: Map.Entry[InetAddress, DatagramChannel] <- ipMap.entrySet()) {
      portMap.put(entry.getValue(), Port.apply(entry.getKey(), ipMap.values()));
    }

    new Router(portMap, ipMap)
  }
}

class Router private (
  val portMap: Map[DatagramChannel, Port],
  val ipMap: Map[InetAddress, DatagramChannel]
) {
  private var lastPacket: Long = System.currentTimeMillis()
  var attachment: Object = null

  def getLastPacket(): Long = lastPacket

  def route(source: InetSocketAddress, channel: DatagramChannel): RouteResult =
    route(source, channel, System.currentTimeMillis())

  def route(
    source: InetSocketAddress,
    channel: DatagramChannel,
    now: Long): RouteResult = {
    val inPort: Port = portMap.get(channel);

    if (inPort == null) {
      return null;
    }

    val outChannel: DatagramChannel = ipMap.get(source.getAddress());

    if (outChannel == null) {
      return null;
    }

    val outPort: Port = portMap.get(outChannel);
    outPort.setRoute(channel, source.getPort());

    val dstIp: InetAddress = inPort.ip
    val dstPort: Option[Int] = inPort.getRoute(outChannel);

    if (!dstPort.isDefined) {
      return null;
    }

    lastPacket = now;
    return new RouteResult(new InetSocketAddress(dstIp, dstPort.get), outChannel);
  }
}