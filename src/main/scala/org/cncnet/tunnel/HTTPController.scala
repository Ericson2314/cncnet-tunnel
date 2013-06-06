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
import java.net.InetSocketAddress

import scala.collection.JavaConversions._
import scala.Option._
import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import scala.collection.mutable.Map
import scala.collection.mutable.Set
import scala.collection.mutable.SynchronizedSet

final class HTTPController private (
  private val dispatcher: Dispatcher,

  private val pool: BlockingQueue[DatagramChannel],

  private val name: String,
  private val password: Option[String],
  private val maxClients: Int,
  private val port: Int,
  private val master: Option[String],
  private val masterPW: Option[String]
) extends TunnelController(dispatcher, pool) with HttpHandler {

  def this(
    dispatcher: Dispatcher,
    channels: Collection[DatagramChannel],
    name: String,
    password: Option[String],
    port: Int,
    maxclients: Int,
    master: Option[String],
    masterpw: Option[String]
  ) = this(
    dispatcher,

    new ArrayBlockingQueue[DatagramChannel](channels.size, true, channels),

    name,
    password,
    maxclients,
    port,
    master,
    masterpw
  )

  private def handleRequest(t: HttpExchange) {
    val params: Array[String] = {
      val query = t.getRequestURI().getQuery()
      if (query == null) new Array[String](0) else query.split("&") // this is implicitly returned
    }
    val requestAddress: InetAddress = t.getRemoteAddress().getAddress()

    var pwOk: Boolean = !password.isDefined

    val addresses: HashSet[InetAddress] = new HashSet[InetAddress]

    for (param: String <- params) {
      val kv: Array[String] = param.split("=")
      if (kv.length == 2) {

        kv(0) = URLDecoder.decode(kv(0), "UTF-8")
        kv(1) = URLDecoder.decode(kv(1), "UTF-8")

        if (kv(0).equals("password") && password.isDefined && kv(1).equals(password)) {
          pwOk = true
        }

        if (kv(0) == "ip[]") {
          val newAddress: InetAddress = InetAddress.getByName(kv(1))
          if (addresses.contains(newAddress)) {
            dispatcher.logger.log("The same addresss was given twice")
            t.sendResponseHeaders(400, 0)
            t.getResponseBody().close()
            return
          }
          if (newAddress != null) {
            addresses.add(newAddress)
          }
        }
      }
    }

    if (!pwOk) {
      // Unauthorized
      dispatcher.logger.log("Request was unauthorized.")
      t.sendResponseHeaders(401, 0)
      t.getResponseBody().close()
      return
    }

    try {
      val newGroup: Group = super.addGroup(requestAddress, addresses)

      val ret: StringBuilder = new StringBuilder()
      for ((addr, alias) <- newGroup.addrToAlias) ret.append(addr.toString().substring(1) + " " + alias.socket().getLocalPort() + "\n")

      respondHelper(200, ret, t)
    } catch {
      case e: ControllerException => {
        dispatcher.logger.log(e.getLocalizedMessage) // log error message
        e match {                                    // then...
          case e: RepeatedAddress => blankRespondHelper(400, t)
          case e: BadNumAddresses => blankRespondHelper(400, t)
          case e: RequestSpam     => blankRespondHelper(429, t)
          case e: DepletedPool    => blankRespondHelper(503, t)
        }
      }
    }
  }

  private def blankRespondHelper(responseCode: Int, t: HttpExchange) {
    t.sendResponseHeaders(responseCode, 0)
    t.getResponseBody().close()
  }

  private def respondHelper(responseCode: Int, response: CharSequence, t: HttpExchange) {
    t.sendResponseHeaders(responseCode, response.length())
    val os: OutputStream = t.getResponseBody()
    os.write(response.toString.getBytes())
    os.close()
  }

  private def handleStatus(t: HttpExchange) {
    val response: String = pool.size() + " slots free.\n" + dispatcher.numRouters + " slots in use.\n"
    dispatcher.logger.log("Response: " + response)
    respondHelper(200, response, t)
  }

  def handle(t: HttpExchange) {
    val uri: String = t.getRequestURI().toString()
    t.getRequestBody().close()

    dispatcher.logger.log("HTTPRequest: " + uri)

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
        dispatcher.logger.log("Error: " + e.getMessage())
        val error: String = e.getMessage()
        respondHelper(500, error, t)
    }
  }

  val HEARTBEAT_PERIOD = 60000

  def run() {
    var lastHeartbeat: Long = 0

    dispatcher.logger.status("Connecting...")

    dispatcher.logger.log("TunnelController started.")

    var connected: Boolean = false

    while (true) {

      val now: Long = System.currentTimeMillis()

      if (lastHeartbeat + HEARTBEAT_PERIOD < now && master.isDefined) {
        dispatcher.logger.log("Sending a heartbeat to master server.")

        connected = false
        try {
          val url: URL = new URL(
            master.get + "?"
              + "name=" + URLEncoder.encode(name, "US-ASCII")
              + "&password=" + (if (password.isDefined) "1" else "0")
              + "&port=" + port
              + "&clients=" + dispatcher.numRouters
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
          case e: FileNotFoundException => dispatcher.logger.log("Master server reported error 404.")
          case e: MalformedURLException => dispatcher.logger.log("Failed to send heartbeat: " + e.toString())
          case e: IOException           => dispatcher.logger.log("Failed to send heartbeat: " + e.toString())
        }

        lastHeartbeat = now
      }

      for ((channel, router) <- dispatcher.routerKVs) {

        if (router.getLastPacket() + HEARTBEAT_PERIOD < now) {
          dispatcher.logger.log("Port " + channel.socket().getLocalPort() + " timed out from router " + router.hashCode() + ".")
          pool.add(channel)

          locks.remove(router) // OK if lock is not present in set
        }
      }

      dispatcher.logger.status(
        (if (connected) "Connected. " else "Disconnected from master. ") +
          dispatcher.numRouters + " / " + maxClients + " players online.")

      try {
        Thread.sleep(5000)
      } catch {
        case _: InterruptedException => () // catch and ignore
      }
    }
  }
}
