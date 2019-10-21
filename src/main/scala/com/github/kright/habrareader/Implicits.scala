package com.github.kright.habrareader

import java.io.{BufferedWriter, File, FileWriter}

import scala.io.Source
import scala.util.{Failure, Success, Try}

object Implicits {

  implicit class CloseableExt[T <: AutoCloseable](private val a: T) extends AnyVal {

    def use[R](func: T => R): R = {
      Try(func(a)) match {
        case Success(result) =>
          a.close()
          result
        case Failure(exception) =>
          Try(a.close()).failed.foreach(ex => exception.addSuppressed(ex))
          throw exception
      }
    }
  }

  implicit class FileExt(private val file: File) extends AnyVal {
    def text: String = Source.fromFile(file).use {
      _.getLines().mkString("\n")
    }

    def text_=(newText: String): Unit = {
      val bw = new BufferedWriter(new FileWriter(file))
      bw.write(newText)
      bw.close()
    }
  }
}
