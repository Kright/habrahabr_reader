package com.github.kright.habrareader.models

import com.github.kright.habrareader.utils.DateUtils
import org.scalatest.FunSuite

import scala.collection.mutable


class ChatDataTest extends FunSuite {

  def checkConsistency(initial: ChatData) = {
    val encoded1 = ChatData.encode(initial)
    val decoded = ChatData.decode(encoded1)
    val encoded2 = ChatData.encode(decoded)

    assert(encoded1 == encoded2)
    assert(ChatData.chatDataEq.eqv(decoded, initial), s"$decoded != $initial")
  }

  test("testEncodeAndDecodeEmpty") {
    val empty = ChatData.empty()

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
      metrics = Some(new ArticleMetrics(
        upVotes = 1,
        downVotes = 2,
        viewsCount = 3,
        commentsCount = 4,
        bookmarksCount = 5)),
      publicationDate = DateUtils.now,
      lastUpdateTime = DateUtils.now,
    )

    checkConsistency(new ChatData(chats, articles))
  }

}
