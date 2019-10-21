package com.github.awant.habrareader.models

import java.io.File
import java.util.Date

import cats.Eq
import com.github.awant.habrareader.Implicits._
import io.circe.syntax._
import io.circe.{Encoder, Json, _}
import org.slf4j.LoggerFactory

import scala.collection.mutable

object ChatData {

  case class Update(chat: Chat, article: HabrArticle, prevMessageId: Option[Int] = None) {
    def date: Date = article.lastUpdateTime
  }

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

  implicit val chatDataEncoder: Encoder[ChatData] = (chatData: ChatData) =>
    Json.obj(
      "chats" := chatData.chats,
      "articles" := chatData.articles,
    )

  implicit val chatDataDecoder: Decoder[ChatData] = (c: HCursor) =>
    for {
      chats <- c.get[mutable.HashMap[Long, Chat]]("chats")
      articles <- c.get[mutable.HashMap[Long, HabrArticle]]("articles")
    } yield new ChatData(chats, articles)

  implicit val chatDataEq = new Eq[ChatData] {
    override def eqv(x: ChatData, y: ChatData): Boolean =
      x.articles == y.articles && x.chats == y.chats
  }
}

class ChatData(private val chats: mutable.HashMap[Long, Chat],
               private val articles: mutable.HashMap[Long, HabrArticle]) {

  private val log = LoggerFactory.getLogger(classOf[ChatData])

  def getChat(id: Long): Chat = chats.getOrElse(id, Chat.withDefaultSettings(id))

  def updateChat(chatId: Long)(mapFunc: Chat => Chat): Unit =
    chats(chatId) = mapFunc(chats.getOrElse(chatId, Chat.withDefaultSettings(chatId)))

  private def predicate(chat: Chat, article: HabrArticle): Boolean = {
    def getMeanOrZero(numbers: Seq[Double]): Double =
      if (numbers.isEmpty)
        0
      else
        numbers.sum / numbers.size

    val weight =
      article.metrics.map(m => m.upVotes - m.downVotes).getOrElse(0) +
        chat.filterSettings.authorWeights.getOrElse(article.author, 0.0) +
        getMeanOrZero(article.categories.toSeq.map(chat.filterSettings.tagWeights.getOrElse(_, 0.0)))

    weight >= chat.filterSettings.ratingThreshold
  }

  private def getSentArticlesUpdates(): Iterable[ChatData.Update] =
    chats.values
      .filter(_.filterSettings.updateAsSoonAsPossible)
      .flatMap { chat =>
        chat.sentArticles.values.flatMap { sentArticle =>
          articles.get(sentArticle.articleId)
            .filter(_.lastUpdateTime.after(sentArticle.sentDate))
            .map(pc => ChatData.Update(chat, pc, Some(sentArticle.messageId)))
        }
      }

  private def getNewArticlesUpdates(): Iterable[ChatData.Update] = {
    chats.values
      .filter(_.filterSettings.updateAsSoonAsPossible)
      .flatMap { chat =>
        articles.values
          .filter(article => !chat.sentArticles.contains(article.id) && predicate(chat, article))
          .map(article => ChatData.Update(chat, article, None))
      }
  }

  def getUpdates(fromDate: Date): Iterable[ChatData.Update] =
    getSentArticlesUpdates() ++ getNewArticlesUpdates()

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
