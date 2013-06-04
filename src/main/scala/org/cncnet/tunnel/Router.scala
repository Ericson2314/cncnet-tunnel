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
object Port {
  def apply(ip: InetAddress, chanList: Collection[DatagramChannel]): Port = {
    val outMap = new HashMap[DatagramChannel, Int]()
    for (channel: DatagramChannel <- chanList) {
      outMap.put(channel, 0)
    }
    new Port(ip, outMap)
  }
  
  def apply(ip: InetAddress, chanList: java.util.Collection[DatagramChannel]): Port = apply(ip, chanList)
}

class Port private (
  val ip: InetAddress,
  protected val outMap: Map[DatagramChannel, Int] // outMap protected to hide entry removal
) {
  def getRoute(channel: DatagramChannel): Option[Int] = outMap.get(channel)
  def setRoute(channel: DatagramChannel, remotePort: Int): Unit = outMap.put(channel, remotePort)
}

object Router {
  def apply(ipMap: Map[InetAddress, DatagramChannel]): Router = {
    val portMap = new HashMap[DatagramChannel, Port]()

    for ((address: InetAddress, channel: DatagramChannel) <- ipMap) {
      portMap.put(channel, Port.apply(address, ipMap.values))
    }

    new Router(portMap, ipMap)
  }
}

class Router private (
  val portMap: Map[DatagramChannel, Port],
  val ipMap: Map[InetAddress, DatagramChannel]
) {
  private var lastPacket: Long = System.currentTimeMillis()

  def getLastPacket(): Long = lastPacket

  def route(source: InetSocketAddress, channel: DatagramChannel): Option[(InetSocketAddress,DatagramChannel)] =
    route(source, channel, System.currentTimeMillis())

  def route(
    source: InetSocketAddress,
    destChannel: DatagramChannel,
    now: Long
  ): Option[(InetSocketAddress, DatagramChannel)] = (portMap.get(destChannel), ipMap.get(source.getAddress())) match {
    case (Some(inPort), Some(outChannel)) => {

      portMap.get(outChannel) match {
        case Some(outPort) => outPort.setRoute(destChannel, source.getPort())
        case None => return None // error
      }

      val dstIp: InetAddress = inPort.ip

      inPort.getRoute(outChannel) match {
        case Some(dstPort) => { lastPacket = now; Some(new InetSocketAddress(dstIp, dstPort), outChannel) }
        case None => None // error
      }
    }
    case _ => None // error
  }
}