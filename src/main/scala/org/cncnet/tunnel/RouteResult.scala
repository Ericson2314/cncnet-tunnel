package org.cncnet.tunnel

import java.net.InetSocketAddress
import java.nio.channels.DatagramChannel

class RouteResult(
    val destination: InetSocketAddress,
    val channel: DatagramChannel    
)