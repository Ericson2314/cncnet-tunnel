package org.cncnet.tunnel

import java.net.InetAddress
import java.net.InetSocketAddress
import java.nio.ByteBuffer
import java.nio.channels.DatagramChannel

import scala.collection.mutable.HashMap
import scala.collection.mutable.{Map => MMap}
import scala.collection.immutable.{Map => IMap}

/**
 * A group represents a single game. Members of a group will only be able to send packets to each other.
 * A group is immutable
 */
class Group (
  val addrToAlias: IMap[InetAddress, DatagramChannel]               // maps client IPs to router ports
) {
  final val aliasToPort: MMap[DatagramChannel, InetSocketAddress] = // maps router port to client ports (and IPs)
    new HashMap[DatagramChannel, InetSocketAddress]()               // initially empty because client ports are initially unknown

  private var lastPacket: Long = System.currentTimeMillis()
  def getLastPacket(): Long = lastPacket

  /**
   * takes alias of destination and real source, returns alias of source and real destination. Updates maps if need be.
   * @param source    actual source IP of client that sent us packet
   * @param destAlias the router port to which the packet was sent, corresponding to the client they wish to send the packet to
   * @param now       roughly time the packet was received
   */
  def route(
    source: InetSocketAddress,
    destAlias: DatagramChannel,
    now: Long
  ): Option[(InetSocketAddress, DatagramChannel)] = (addrToAlias.get(source.getAddress()), aliasToPort.get(destAlias)) match {
    case (None, _)                           => None // sender not involved with this game, denied
    case (Some(senderAlias), None)           => {    // destination client either has not connected yet, or is not part of the match
      aliasToPort.put(senderAlias, source)           //  - update map with sender's latest port
      None                                           //  - return failure
    }
    case (Some(senderAlias), Some(destPort)) => {    // all good!
      aliasToPort.put(senderAlias, source)           //  - update map with sender's latest port
      lastPacket = now                               //  - update lastPacket with time packet was received
      Some(destPort, senderAlias)                    //  - return mappings
    }
  }
}
