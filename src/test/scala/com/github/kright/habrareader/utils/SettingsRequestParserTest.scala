package com.github.kright.habrareader.utils

import org.scalatest.FunSuite

class SettingsRequestParserTest extends FunSuite {

  import ChangeSettings.{parse => parseSettings}

  test("empty string") {
    assert(ChangeSettings.parse("").isEmpty)
    assert(ChangeSettings.parse("     ").isEmpty)
  }

  test("just author") {
    assert(parseSettings("/author a 1") == Seq(ChangeSettings.AuthorRating("a", 1.0)))
  }

  test("just tag") {
    assert(parseSettings("/tag b 2") == Seq(ChangeSettings.TagRating("b", 2.0)))
  }

  test("just threshold") {
    assert(parseSettings("/rating 25") == Seq(ChangeSettings.RatingThreshold(25)))
  }

  test("subscription") {
    assert(parseSettings("/subscribe") == Seq(ChangeSettings.ChangeSubscription(true)))
    assert(parseSettings("/unsubscribe") == Seq(ChangeSettings.ChangeSubscription(false)))
  }

  test("just reset") {
    assert(parseSettings("/reset") == Seq(ChangeSettings.Reset))
  }

  test("capitalized Cmd") {
    assert(parseSettings("/Rating 25") == Seq(ChangeSettings.RatingThreshold(25)))
  }

  test("capitalized text") {
    assert(parseSettings("/author Aa 2") == Seq(ChangeSettings.AuthorRating("aa", 2.0)))
  }

  test("many comands") {
    assert(parseSettings("/author aa 20 /author BB 1 /tag t1 3 /tag t2.3 4 /rating 2.0") ==
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
    assert(parseSettings("/author aa   20 \t/author \n\r\nBB 1 /tag t1 3 /tag t2.3 4 /rating 2.0") ==
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
