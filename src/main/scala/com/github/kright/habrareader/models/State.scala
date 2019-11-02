package com.github.kright.habrareader.models

import java.io.File

import cats.Eq
import com.github.kright.habrareader.Implicits._
import com.github.kright.habrareader.actors.TgBotActor.UpdateArticle
import io.circe.syntax._
import io.circe.{Encoder, Json, _}
import org.slf4j.LoggerFactory

import scala.collection.mutable

object State {

  def empty(): State =
    new State(new mutable.HashMap(), new mutable.HashMap())

  def decode(s: String): State =
    io.circe.parser.decode[State](s).right.get

  def encode(c: State): String =
    c.asJson.toString()

  def load(file: File): State =
    decode(file.text)

  def save(c: State, file: File): Unit =
    file.text = encode(c)

  private implicit val chatDataEncoder: Encoder[State] = (chatData: State) =>
    Json.obj(
      "chats" := chatData.chats,
      "articles" := chatData.articles,
    )

  private implicit val chatDataDecoder: Decoder[State] = (c: HCursor) =>
    for {
      chats <- c.get[mutable.HashMap[Long, Chat]]("chats")
      articles <- c.get[mutable.HashMap[Int, HabrArticle]]("articles")
    } yield new State(chats, articles)

  implicit val chatDataEq = new Eq[State] {
    override def eqv(x: State, y: State): Boolean =
      x.articles == y.articles && x.chats == y.chats
  }
}

class State(val chats: mutable.HashMap[Long, Chat],
            val articles: mutable.HashMap[Int, HabrArticle]) {

  private val log = LoggerFactory.getLogger(classOf[State])

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

  def updateArticle(newArticle: HabrArticle): Unit =
      articles(newArticle.id) = newArticle

  def addSentArticle(chatId: Long, sentArticle: SentArticle): Unit =
    updateChat(chatId) { chat =>
      chat.copy(sentArticles = chat.sentArticles.updated(sentArticle.articleId, sentArticle))
    }

  override def toString: String =
    State.encode(this)
}
