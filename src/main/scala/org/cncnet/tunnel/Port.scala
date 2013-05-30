package org.cncnet.tunnel

import java.net.InetAddress
import java.nio.channels.DatagramChannel
import java.util.List

import scala.collection.JavaConverters._
import scala.collection.mutable.Map
import scala.collection.mutable.HashMap


object Port {
  def apply(ip: InetAddress, chanList: Collection[DatagramChannel]): Port = {
    val outMap = new HashMap[DatagramChannel, Int]()
    for (channel: DatagramChannel <- chanList) {
      outMap.put(channel, 0)
    }
    new Port(ip, outMap)
  }
  
  def apply(ip: InetAddress, chanList: java.util.Collection[DatagramChannel]): Port = apply(ip, chanList.asScala)
}

class Port private (
  val ip: InetAddress,
  protected val outMap: Map[DatagramChannel, Int] // outMap protected to hide entry removal
) {
  def getRoute(channel: DatagramChannel): Option[Int] = outMap.get(channel)
  def setRoute(channel: DatagramChannel, remotePort: Int): Unit = outMap.put(channel, remotePort)
}