package com.github.kright.habrareader.utils

import com.github.kright.habrareader.utils.TextNormalization.normalize
import org.scalatest.FunSuite

class TextNormalizationTest extends FunSuite {

  test("testNormalize") {
    assert(normalize("text") == "text")
    assert(normalize("lOwEr") == "lower")
    assert(normalize("author name") == "author_name")
    assert(normalize("tag4.1") == "tag4_1")
    assert(normalize("a&b") == "a_b")
  }
}
