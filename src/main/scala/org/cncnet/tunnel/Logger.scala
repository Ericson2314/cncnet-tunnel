package org.cncnet.tunnel

import java.io.File
import java.io.FileOutputStream
import java.io.IOException
import java.util.Date

object Logger {
  def apply(file: Option[String], sw: Option[StatusWindow]): Logger = try {
    new Logger(file.map(f => new FileOutputStream(new File(f), true)), sw)
  } catch {
    case e: IOException => {
      val logger = new Logger(None, sw)
      logger.log("file for logging could not be written to")
      logger // return this from block
    }
  }
}

class Logger (
  val logStream: Option[FileOutputStream],
  val statusWindow: Option[StatusWindow]
) {
  
  def log(msg: String): Unit = log(msg.split("\n"))

  val log: Array[String] => Unit = {
    val extraPrint: String => Unit = (logStream, statusWindow) match {
      case (None, None)            => _ => () // no-op
      case (None, Some(window))    => window.log
      case (Some(_), None)         => appendToFile
      case (Some(_), Some(window)) => line: String => { window.log(line); appendToFile(line) }
    }

    lines: Array[String] => lines.map(rawLine => {
      val goodLine = "[" + new Date().toString() + "] " + rawLine
      System.out.println(goodLine)
      extraPrint(goodLine)
    })
  }

  private def appendToFile(line: String) {
    try {
      logStream.get.write(line.getBytes())
      logStream.get.write('\n')
    } catch {
      case e: IOException => () // silently ignore errors
    }
  }
  
  // wrapper for java
  def status(s: String) = statusHelper(s)
  
  private val statusHelper: String => Unit = statusWindow match {
    case Some(sw) => statusWindow.get.status
    case None     => _ => ()
  }
}