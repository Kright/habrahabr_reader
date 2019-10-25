package com.github.kright.habrareader.utils

object TextNormalization {

  def normalize(s: String): String =
    s.toLowerCase
      .replace('.', '_')
      .replace(' ', '_')
}
