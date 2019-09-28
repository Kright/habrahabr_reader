package com.github.awant.habrareader.utils

import scala.util.Try

object SettingsRequestParser {

  private def tokenize(s: String): Array[String] = s.split(" ").filter(_.nonEmpty)

  private def asTokens(s: String, count: Int): Option[Array[String]] = {
    val arr = tokenize(s)
    if (arr.size == count)
      Some(arr)
    else
      None
  }

  object Command {
    def unapply(text: String): Option[String] =
      asTokens(text, 1).map(_.head)
  }

  object CommandStringDouble {
    def unapply(text: String): Option[(String, String, Double)] =
      asTokens(text, 3).flatMap { tokens =>
        Try {
          (tokens(0), tokens(1), tokens(2).toDouble)
        }.toOption
      }
  }

  object CommandDouble {
    def unapply(text: String): Option[(String, Double)] =
      asTokens(text, 2).flatMap { tokens =>
        Try {
          (tokens(0), tokens(1).toDouble)
        }.toOption
      }
  }

}