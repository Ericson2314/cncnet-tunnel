/*
 * Copyright (c) 2013 Toni Spets [toni.spets@iki.fi]
 * 
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */
package org.cncnet.tunnel

import com.sun.net.httpserver.HttpExchange
import com.sun.net.httpserver.HttpHandler
import java.io.FileNotFoundException
import java.io.IOException
import java.io.OutputStream
import java.net.HttpURLConnection
import java.net.InetAddress
import java.net.MalformedURLException
import java.net.URL
import java.net.URLDecoder
import java.net.URLEncoder
import java.nio.channels.DatagramChannel
import java.util.Iterator
import java.util.concurrent.ArrayBlockingQueue
import java.util.concurrent.BlockingQueue
import java.util.concurrent.ConcurrentHashMap

import scala.collection.JavaConversions._
import scala.Option._
import scala.collection.mutable.HashMap
import scala.collection.mutable.Map

final class TunnelController private (
  private val logger: Logger,

  private val pool: BlockingQueue[DatagramChannel],
  private val routers: Map[DatagramChannel, Router],
  private val locks: Map[String, Router],

  private val name: String,
  private val password: Option[String],
  private val maxClients: Int,
  private val port: Int,
  private val master: Option[String],
  private val masterPW: Option[String]

) extends HttpHandler with Runnable {

  def this(
    logger: Logger,
    channels: Collection[DatagramChannel],
    name: String,
    password: Option[String],
    port: Int,
    maxclients: Int,
    master: Option[String],
    masterpw: Option[String]
  ) = this(
    logger,

    new ArrayBlockingQueue[DatagramChannel](channels.size, true, channels),
    new ConcurrentHashMap[DatagramChannel, Router](),
    new ConcurrentHashMap[String, Router](),

    name,
    password,
    maxclients,
    port,
    master,
    masterpw)

  // will get called by another thread
  def getRouter(channel: DatagramChannel) = routers.get(channel)

  private def handleRequest(t: HttpExchange) {
    val clients: Map[InetAddress, DatagramChannel] = new HashMap[InetAddress, DatagramChannel]()

    val params: Array[String] = {
      val query = t.getRequestURI().getQuery()
      if (query == null) new Array[String](0) else query.split("&") // this is implicitly returned
    }
    val requestAddress: String = t.getRemoteAddress().getAddress().getHostAddress()
    
    var pwOk: Boolean = !password.isDefined;

    val addresses: List[InetAddress] = params.foldLeft[List[InetAddress]](Nil)((addresses, param: String) => {
      val kv: Array[String] = param.split("=");
      if (kv.length == 2) {

        kv(0) = URLDecoder.decode(kv(0), "UTF-8");
        kv(1) = URLDecoder.decode(kv(1), "UTF-8");

        if (kv(0).equals("password") && password.isDefined && kv(1).equals(password)) {
          pwOk = true;
        }

        if (kv(0) == "ip[]") {
          val newAddress: InetAddress = InetAddress.getByName(kv(1));
          if (newAddress != null) {
            addresses.::(newAddress)
          } else addresses
        } else addresses
      } else addresses
    })
    
    if (!pwOk) {
      // Unauthorized
      logger.log("Request was unauthorized.")
      t.sendResponseHeaders(401, 0)
      t.getResponseBody().close()
      return
    }

    if (addresses.size < 2 || addresses.size > 8) {
      // Bad Request
      logger.log("Request had invalid amount of addresses.")
      t.sendResponseHeaders(400, 0)
      t.getResponseBody().close()
      return
    }

    // lock the request ip out until this router is collected
    if (locks.containsKey(requestAddress)) {
      // Too Many Requests
      logger.log("Same address tried to request more than one active router.")
      t.sendResponseHeaders(429, 0)
      t.getResponseBody().close()
      return
    }

    val ret: StringBuilder = new StringBuilder()

    try {
      for (address: InetAddress <- addresses) {
        val channel: DatagramChannel = pool.remove()
        ret.append(address.toString().substring(1) + " " + channel.socket().getLocalPort() + "\n")
        clients.put(address, channel)
      }
    } catch {
      case e: NoSuchElementException => {
        // if not enough 
        pool.addAll(clients.values())
        // Service Unavailable
        logger.log("Request wanted more than we could provide.")
        t.sendResponseHeaders(503, 0)
        t.getResponseBody().close()
        return
      }
    }

    val router: Router = Router(clients)
    router.attachment = requestAddress

    // lock the request ip out until this router is collected
    locks.put(t.getRemoteAddress().getAddress().getHostAddress(), router)

    for (( address: InetAddress, channel: DatagramChannel)  <- clients) {
      logger.log("Port " + channel.socket().getLocalPort() + " allocated for " + address.toString() + " in router " + router.hashCode() + ".")
      routers.put(channel, router)
    }

    t.sendResponseHeaders(200, ret.length())
    val os: OutputStream = t.getResponseBody()
    os.write(ret.toString().getBytes())
    os.close()
  }

  private def handleStatus(t: HttpExchange) {
    val response: String = pool.size() + " slots free.\n" + routers.size + " slots in use.\n"
    logger.log("Response: " + response)
    t.sendResponseHeaders(200, response.length())
    val os: OutputStream = t.getResponseBody()
    os.write(response.getBytes())
    os.close()
  }

  def handle(t: HttpExchange) {
    val uri: String = t.getRequestURI().toString()
    t.getRequestBody().close()

    logger.log("HTTPRequest: " + uri)

    try {
      if (uri.startsWith("/request")) {
        handleRequest(t)
      } else if (uri.startsWith("/status")) {
        handleStatus(t)
      } else {
        t.sendResponseHeaders(400, 0)
      }
    } catch {
      case e: IOException =>
        logger.log("Error: " + e.getMessage())
        val error: String = e.getMessage()
        t.sendResponseHeaders(500, error.length())
        val os: OutputStream = t.getResponseBody()
        os.write(error.getBytes())
        os.close()
    }
  }

  val HEARTBEAT_PERIOD = 60000

  def run() {
    var lastHeartbeat: Long = 0

    logger.status("Connecting...")

    logger.log("TunnelController started.")

    var connected: Boolean = false

    while (true) {

      val now: Long = System.currentTimeMillis()

      if (lastHeartbeat + HEARTBEAT_PERIOD < now && master.isDefined) {
        logger.log("Sending a heartbeat to master server.")

        connected = false
        try {
          val url: URL = new URL(
            master.get + "?"
              + "name=" + URLEncoder.encode(name, "US-ASCII")
              + "&password=" + (if (password.isDefined) "1" else "0")
              + "&port=" + port
              + "&clients=" + routers.size
              + "&maxclients=" + maxClients
              + (masterPW match {
                case Some(pw) => "&masterpw=" + URLEncoder.encode(pw, "US-ASCII")
                case None => ""
              }))
          val con: HttpURLConnection = url.openConnection() match {
            case c: HttpURLConnection => c
            case _ => throw new ClassCastException()
          }
          con.setRequestMethod("GET")
          con.setConnectTimeout(5000)
          con.setReadTimeout(5000)
          con.connect()
          con.getInputStream().close()
          con.disconnect()
          connected = true
        } catch {
          case e: FileNotFoundException => logger.log("Master server reported error 404.")
          case e: MalformedURLException => logger.log("Failed to send heartbeat: " + e.toString())
          case e: IOException => logger.log("Failed to send heartbeat: " + e.toString())
        }

        lastHeartbeat = now
      }
      
      for ((channel, router) <- routers) {

        if (router.getLastPacket() + HEARTBEAT_PERIOD < now) {
          logger.log("Port " + channel.socket().getLocalPort() + " timed out from router " + router.hashCode() + ".")
          pool.add(channel)

          if (locks.containsKey(router.attachment)) locks.removeKey(router.attachment.toString)
        }
      }

      logger.status (
        (if (connected) "Connected. " else "Disconnected from master. ") +
        routers.size + " / " + maxClients + " players online."
      )

      try {
        Thread.sleep(5000)
      } catch {
        case _: InterruptedException => () // catch and ignore
      }
    }
  }
}