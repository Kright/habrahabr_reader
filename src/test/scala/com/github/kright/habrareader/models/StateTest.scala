package com.github.kright.habrareader.models

import com.github.kright.habrareader.utils.DateUtils
import org.scalatest.FunSuite

import scala.collection.mutable


class StateTest extends FunSuite {

  def checkConsistency(initial: State): Unit = {
    val encoded1 = State.encode(initial)
    val decoded = State.decode(encoded1)
    val encoded2 = State.encode(decoded)

    assert(encoded1 == encoded2)
    assert(State.chatDataEq.eqv(decoded, initial), s"$decoded != $initial")
  }

  test("testEncodeAndDecodeEmpty") {
    val empty = State.empty()

    checkConsistency(empty)
  }

  test("testEncodeAndDecoder") {
    val chats = new mutable.HashMap[Long, Chat]()
    val articles = new mutable.HashMap[Int, HabrArticle]()

    chats(1) = Chat.withDefaultSettings(1)
    chats(2) = Chat.withDefaultSettings(2)

    articles(-3) = new HabrArticle(
      id = -3,
      link = "link",
      title = "test atricle",
      description = "decr",
      author = "authorName",
      categories = Set("kek", "lol"),
      company = None,
      metrics = Some(new ArticleMetrics(
        upVotes = 1,
        downVotes = 2,
        viewsCount = 3,
        commentsCount = 4,
        bookmarksCount = 5)),
      publicationDate = DateUtils.now,
      lastUpdateTime = DateUtils.now,
    )

    checkConsistency(new State(chats, articles))
  }

}
