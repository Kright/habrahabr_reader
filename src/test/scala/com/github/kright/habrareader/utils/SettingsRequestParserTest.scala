package com.github.kright.habrareader.utils

import org.scalatest.FunSuite

class SettingsRequestParserTest extends FunSuite {

  import ChangeSettings.parse

  test("empty string") {
    assert(parse("").isEmpty)
    assert(parse("     ").isEmpty)
  }

  test("just author") {
    assert(parse("/author a 1") == Seq(ChangeSettings.AuthorRating("a", 1.0)))
  }

  test("just tag") {
    assert(parse("/tag b 2") == Seq(ChangeSettings.TagRating("b", 2.0)))
  }

  test("just threshold") {
    assert(parse("/rating 25") == Seq(ChangeSettings.RatingThreshold(25)))
  }

  test("subscription") {
    assert(parse("/subscribe") == Seq(ChangeSettings.ChangeSubscription(true)))
    assert(parse("/unsubscribe") == Seq(ChangeSettings.ChangeSubscription(false)))
  }

  test("just reset") {
    assert(parse("/reset") == Seq(ChangeSettings.Reset))
  }

  test("capitalized Cmd") {
    assert(parse("/Rating 25") == Seq(ChangeSettings.RatingThreshold(25)))
  }

  test("capitalized text") {
    assert(parse("/author Aa 2") == Seq(ChangeSettings.AuthorRating("aa", 2.0)))
  }

  test("many comands") {
    assert(parse("/author aa 20 /author BB 1 /tag t1 3 /tag t2.3 4 /rating 2.0") ==
      Seq(
        ChangeSettings.AuthorRating("aa", 20),
        ChangeSettings.AuthorRating("bb", 1),
        ChangeSettings.TagRating("t1", 3),
        ChangeSettings.TagRating("t2_3", 4),
        ChangeSettings.RatingThreshold(2),
      )
    )
  }

  test("many comands with crazy whitespaces") {
    assert(parse("/author aa   20 \t/author \n\r\nBB 1 /tag t1 3 /tag t2.3 4 /rating 2.0") ==
      Seq(
        ChangeSettings.AuthorRating("aa", 20),
        ChangeSettings.AuthorRating("bb", 1),
        ChangeSettings.TagRating("t1", 3),
        ChangeSettings.TagRating("t2_3", 4),
        ChangeSettings.RatingThreshold(2),
      )
    )
  }
}
