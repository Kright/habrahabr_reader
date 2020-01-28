package com.github.kright.habrareader.actors

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import com.github.kright.habrareader.AppConfig.LibraryActorConfig
import com.github.kright.habrareader.actors.LibraryActor._
import com.github.kright.habrareader.actors.TgBotActor.SendMessageToTg
import com.github.kright.habrareader.models._
import com.github.kright.habrareader.utils.{DateUtils, Saver}

import scala.concurrent.ExecutionContextExecutor


object LibraryActor {
  def props(config: LibraryActorConfig, saver: Saver[State]): Props = Props(new LibraryActor(config, saver))

  final case class PostWasSentToTg(chatId: Long, sentArticle: SentArticle)
  final case class GetSettings(chatId: Long)
  final case class UpdateChat(chatId: Long, updater: Chat => Chat, needConfirmation: Boolean)
  final case class RequestUpdates(chatId: Long)
  final case class RequestUpdatesForAll(updateExistingMessages: Boolean)
  final case class UpdateArticle(article: HabrArticle)
  final case class SaveState(needConfirmation: Boolean)
  final case class GetStats(chatId: Long)
  final case object GetArticles
  final case class AllArticles(articles: Iterable[HabrArticle])
  final case object Ok
}

class LibraryActor(config: LibraryActorConfig, saver: Saver[State]) extends Actor with ActorLogging {
  implicit val executionContext: ExecutionContextExecutor = context.dispatcher

  private val chatData = saver.load()

  override def preStart(): Unit = {
    context.system.scheduler.schedule(config.stateSaveInterval, config.stateSaveInterval, self, SaveState(needConfirmation = false))
  }

  override def receive: Receive = {
    case UpdateChat(chatId, updater, needConfirmation) =>
      chatData.updateChat(chatId)(updater)
      if (needConfirmation) {
        sender ! Ok
      }
    case GetSettings(chatId) =>
      sender ! SendMessageToTg(chatId, chatData.getChat(chatId).getSettingsAsCmd)
    case RequestUpdatesForAll(updateExistingMessages) =>
      processNewPostSending(sender, updateExistingMessages)
    case UpdateArticle(article) =>
      chatData.updateArticle(article)
    case PostWasSentToTg(chatId, sentArticle) =>
      chatData.addSentArticle(chatId, sentArticle)
    case SaveState(needConfirmation) =>
      rmOldArticles()
      saver.save(chatData)
      if (needConfirmation) {
        sender ! Ok
      }
    case RequestUpdates(chatId) =>
      requestUpdates(chatId, sender)
    case GetArticles =>
      sender ! AllArticles(chatData.articles.values.toVector)
    case GetStats(chatId) =>
      sender ! SendMessageToTg(chatId, getStatsMsg)
    case unknownMessage =>
      log.error(s"unknown message: $unknownMessage")
  }

  private def rmOldArticles(): Unit = {
    val threshold = DateUtils.now.getTime - config.timeBeforeRmOldArticles.toMillis
    rmArticles(chatData.articles.values.filter(_.publicationDate.getTime < threshold).map(_.id).toSeq)
  }

  private def rmArticles(idsToRm: Seq[Int]): Unit = {
    chatData.articles --= idsToRm

    chatData.chats.keys.foreach { id =>
      chatData.updateChat(id) { chat =>
        chat.copy(sentArticles = chat.sentArticles -- idsToRm)
      }
    }
  }

  private def getStatsMsg: String =
    s"""articles: <b>${chatData.articles.size}</b>
       |users: <b>${chatData.chats.size}</b>
       |subscribed users: <b>${chatData.chats.values.count(_.filterSettings.updateAsSoonAsPossible)}</b>
      """.stripMargin

  private def processNewPostSending(tgBot: ActorRef, updateExistingMessages: Boolean): Unit = {
    for ((id, chat) <- chatData.chats) {
      if (chat.filterSettings.updateAsSoonAsPossible) {
        chatData.getNewArticles(id).foreach(tgBot ! _)
      }
    }
    // at first send all new articles
    for((id, chat) <- chatData.chats) {
      if (chat.filterSettings.updateAsSoonAsPossible && updateExistingMessages) {
        chatData.getSentArticleUpdates(id).foreach(tgBot ! _)
      }
    }
  }

  private def requestUpdates(chatId: Long, tgBot: ActorRef): Unit = {
    val updates = chatData.getNewArticles(chatId)

    updates.view.take(3).foreach(tgBot ! _)

    if (updates.isEmpty) {
      tgBot ! SendMessageToTg(chatId, "no new articles :(")
    }
  }
}
