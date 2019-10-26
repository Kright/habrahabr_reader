package com.github.kright.habrareader.models

import java.io.File

import cats.Eq
import com.github.kright.habrareader.Implicits._
import com.github.kright.habrareader.actors.TgBotActor.UpdateArticle
import io.circe.syntax._
import io.circe.{Encoder, Json, _}
import org.slf4j.LoggerFactory

import scala.collection.mutable

object ChatData {

  def empty(): ChatData =
    new ChatData(new mutable.HashMap(), new mutable.HashMap())

  def decode(s: String): ChatData =
    io.circe.parser.decode[ChatData](s).right.get

  def encode(c: ChatData): String =
    c.asJson.toString()

  def load(file: File): ChatData =
    decode(file.text)

  def save(c: ChatData, file: File): Unit =
    file.text = encode(c)

  private implicit val chatDataEncoder: Encoder[ChatData] = (chatData: ChatData) =>
    Json.obj(
      "chats" := chatData.chats,
      "articles" := chatData.articles,
    )

  private implicit val chatDataDecoder: Decoder[ChatData] = (c: HCursor) =>
    for {
      chats <- c.get[mutable.HashMap[Long, Chat]]("chats")
      articles <- c.get[mutable.HashMap[Long, HabrArticle]]("articles")
    } yield new ChatData(chats, articles)

  implicit val chatDataEq = new Eq[ChatData] {
    override def eqv(x: ChatData, y: ChatData): Boolean =
      x.articles == y.articles && x.chats == y.chats
  }
}

class ChatData(val chats: mutable.HashMap[Long, Chat],
               val articles: mutable.HashMap[Long, HabrArticle]) {

  private val log = LoggerFactory.getLogger(classOf[ChatData])

  def getChat(id: Long): Chat = chats.getOrElse(id, Chat.withDefaultSettings(id))

  def updateChat(chatId: Long)(mapFunc: Chat => Chat): Unit = {
    chats(chatId) = mapFunc(chats.getOrElse(chatId, Chat.withDefaultSettings(chatId)))
    assert(chats(chatId).id == chatId)
  }

  def getSentArticleUpdates(chatId: Long): Iterable[UpdateArticle] = {
    val chat = getChat(chatId)

    chat.sentArticles.values.flatMap { sentArticle =>
      articles.get(sentArticle.articleId)
        .filter(_.lastUpdateTime.after(sentArticle.sentDate))
        .map(pc => UpdateArticle(chatId, pc, Some(sentArticle.messageId)))
    }
  }

  def getNewArticles(chatId: Long): Iterable[UpdateArticle] = {
    val chat = getChat(chatId)

    articles.values
      .filter(article => !chat.sentArticles.contains(article.id) && chat.filterSettings.isInteresting(article))
      .map(article => UpdateArticle(chatId, article, None))
  }

  def updatePosts(posts: Seq[HabrArticle]): Unit =
    posts.foreach { post =>
      articles(post.id) = post
    }

  def addSentArticle(chatId: Long, sentArticle: SentArticle): Unit =
    updateChat(chatId) { chat =>
      chat.copy(sentArticles = chat.sentArticles.updated(sentArticle.articleId, sentArticle))
    }

  override def toString: String =
    ChatData.encode(this)
}
