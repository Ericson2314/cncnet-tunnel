package org.cncnet.tunnel

import com.sun.net.httpserver.{HttpExchange, HttpHandler}

import java.util.concurrent.{ArrayBlockingQueue, BlockingQueue}
import java.net.InetAddress
import java.nio.channels.DatagramChannel

import scala.collection.JavaConversions._
import scala.collection.mutable.{HashMap, HashSet, Map => MMap, Set, SynchronizedSet}
import scala.collection.immutable.{Map => IMap}

abstract class TunnelController(
  dispatcher: Dispatcher,
  pool: BlockingQueue[DatagramChannel]
) extends Runnable {
  protected val locks: Set[InetAddress] = new HashSet[InetAddress]() with SynchronizedSet[InetAddress]

  def addGroup(requestAddress: InetAddress, inAddrs: Iterable[InetAddress]): Group =  {
    val outAddrs: HashSet[InetAddress] = new HashSet[InetAddress]
    for (addr <- inAddrs) {
      if (outAddrs.contains(addr)) throw new RepeatedAddress
      outAddrs.add(addr)
    }
    addGroup(requestAddress, outAddrs)
  }

  def addGroup(requestAddress: InetAddress, addresses: Set[InetAddress]): Group =  {
    if (addresses.size < 2 || addresses.size > 8) throw new BadNumAddresses
    if (locks.contains(requestAddress))           throw new RequestSpam

    val ret: StringBuilder = new StringBuilder()
    val clients: MMap[InetAddress, DatagramChannel] = new HashMap[InetAddress, DatagramChannel]()

    try {
      for (address: InetAddress <- addresses) {
        val channel: DatagramChannel = pool.remove()
        ret.append(address.toString().substring(1) + " " + channel.socket().getLocalPort() + "\n")
        clients.put(address, channel)
      }
    } catch {
      case e: NoSuchElementException => {pool.addAll(clients.values); throw new DepletedPool}
    }

    val group: Group = new Group(clients.toMap) // convert to immutable map, as groups are immutable

    // lock the request ip out until this router is collected
    locks.add(requestAddress)

    for ((address: InetAddress, clientAlias: DatagramChannel) <- clients) {
      dispatcher.logger.log("Port " + clientAlias.socket().getLocalPort() + " allocated for " + address.toString() + " in router " + group.hashCode() + ".")
      dispatcher.addRouter(clientAlias, group)
    }

    group // return group in case sub-class needs it
  }
}

abstract sealed class ControllerException(val msg: String) extends Exception(msg)
final case class RepeatedAddress extends ControllerException("The same addresss was given twice."                        )
final case class BadNumAddresses extends ControllerException("Request had invalid amount of addresses."                  )
final case class RequestSpam     extends ControllerException("Same address tried to request more than one active router.")
final case class DepletedPool    extends ControllerException("Request wanted more than we could provide."                )
