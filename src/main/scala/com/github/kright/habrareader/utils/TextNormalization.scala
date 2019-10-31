package com.github.kright.habrareader.utils

object TextNormalization {

  def normalize(s: String): String =
    s.toLowerCase
    .map(c => if (c.isLetterOrDigit) c else '_')
}
