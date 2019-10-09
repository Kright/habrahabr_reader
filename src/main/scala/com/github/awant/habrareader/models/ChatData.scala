package com.github.awant.habrareader.models

import java.io.File
import java.util.Date

import org.slf4j.LoggerFactory

import scala.collection.mutable

object ChatData {

  case class Update(chat: Chat, article: HabrArticle, prevMessageId: Option[Int] = None) {
    def date: Date = article.lastUpdateTime
  }

  def empty(): ChatData =
    new ChatData(new mutable.HashMap(), new mutable.HashMap())

  def load(file: File): ChatData = {
    // todo
    ???
  }
}

class ChatData(chatCollection: mutable.HashMap[Long, Chat],
               postCollection: mutable.HashMap[Long, HabrArticle]) {

  private val log = LoggerFactory.getLogger(classOf[ChatData])

  def getChat(id: Long): Chat = chatCollection.getOrElse(id, Chat.withDefaultSettings(id))

  def updateChat(chatId: Long)(mapFunc: Chat => Chat): Unit =
    chatCollection(chatId) = mapFunc(chatCollection.getOrElse(chatId, Chat.withDefaultSettings(chatId)))

  private def predicate(chat: Chat, article: HabrArticle): Boolean = {
    def getMeanOrZero(numbers: Seq[Double]): Double =
      if (numbers.isEmpty)
        0
      else
        numbers.sum / numbers.size

    val weight =
      article.metrics.map(m => m.upVotes - m.downVotes).getOrElse(0) +
        chat.authorWeights.getOrElse(article.author, 0.0) +
        getMeanOrZero(article.categories.toSeq.map(chat.tagWeights.getOrElse(_, 0.0)))

    weight >= chat.ratingThreshold
  }

  private def getSentArticlesUpdates(): Iterable[ChatData.Update] =
    chatCollection.values
      .filter(_.subscription)
      .flatMap { chat =>
        chat.sentArticles.values.flatMap { sentArticle =>
          postCollection.get(sentArticle.articleId)
            .filter(_.lastUpdateTime.after(sentArticle.sentDate))
            .map(pc => ChatData.Update(chat, pc, Some(sentArticle.messageId)))
        }
      }

  private def getNewArticlesUpdates(): Iterable[ChatData.Update]  = {
    chatCollection.values
      .filter(_.subscription)
      .flatMap { chat =>
        postCollection.values
          .filter(article => !chat.sentArticles.contains(article.id) && predicate(chat, article))
          .map(article => ChatData.Update(chat, article, None))
      }
  }

  def getUpdates(fromDate: Date): Iterable[ChatData.Update]  =
    getSentArticlesUpdates() ++ getNewArticlesUpdates()

  def updatePosts(posts: Seq[HabrArticle]): Unit =
    posts.foreach{ post =>
      postCollection(post.id) = post
    }

  def addSentArticle(chatId: Long, sentArticle: SentArticle): Unit =
    updateChat(chatId) { chat =>
      chat.copy(sentArticles = chat.sentArticles.updated(sentArticle.articleId, sentArticle))
    }
}
