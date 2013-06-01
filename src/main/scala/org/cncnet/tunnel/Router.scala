package org.cncnet.tunnel

import java.net.InetAddress
import java.net.InetSocketAddress
import java.nio.channels.DatagramChannel

import scala.collection.JavaConversions._
import scala.collection.mutable.HashMap
import scala.collection.mutable.Map

/**
 *
 * @author Toni Spets <toni.spets@iki.fi>
 */
object Router {
  def apply(ipMap: Map[InetAddress, DatagramChannel]): Router = {
    val portMap = new HashMap[DatagramChannel, Port]()

    for ((address: InetAddress, channel: DatagramChannel) <- ipMap) {
      portMap.put(channel, Port.apply(address, ipMap.values));
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

  def route(source: InetSocketAddress, channel: DatagramChannel): Option[RouteResult] =
    route(source, channel, System.currentTimeMillis())

  def route(
    source: InetSocketAddress,
    channel: DatagramChannel,
    now: Long
  ): Option[RouteResult] = (portMap.get(channel), ipMap.get(source.getAddress())) match {
    case (Some(inPort), Some(outChannel)) => {

      portMap.get(outChannel) match {
        case Some(outPort) => outPort.setRoute(channel, source.getPort());
        case None => return None // error
      }

      val dstIp: InetAddress = inPort.ip

      inPort.getRoute(outChannel) match {
        case Some(dstPort) => { lastPacket = now; Some(new RouteResult(new InetSocketAddress(dstIp, dstPort), outChannel)) }
        case None => None // error
      }
    }
    case _ => None // error
  }
}