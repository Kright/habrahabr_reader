package com.github.kright.habrareader.utils

object StringUtils {
  def decapitalize(string: String): String = {
    if (string.isEmpty) string
    else {
      val chars = string.toCharArray
      chars(0) = Character.toLowerCase(chars(0))
      chars.toString
    }
  }
}
