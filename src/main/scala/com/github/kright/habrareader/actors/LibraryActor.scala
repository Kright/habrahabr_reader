package com.github.kright.habrareader.actors

import java.util.Date

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import com.github.kright.habrareader.AppConfig.LibraryActorConfig
import com.github.kright.habrareader.actors.TgBotActor.Reply
import com.github.kright.habrareader.models._
import com.github.kright.habrareader.utils.SettingsRequestParser.{Command, CommandDouble, CommandStringDouble}
import com.github.kright.habrareader.utils.{DateUtils, SavesDir}

import scala.concurrent.ExecutionContextExecutor


object LibraryActor {
  def props(config: LibraryActorConfig): Props = Props(new LibraryActor(config))

  final case class PostWasSentToTg(chatId: Long, sentArticle: SentArticle)
  final case class GetSettings(chatId: Long)
  final case class ChangeSettings(chatId: Long, body: String)
  final case class RequestUpdates(chatId: Long)
  final case object RequestUpdatesForTg
  final case class UpdateArticles(articles: Seq[HabrArticle])
  final case object SaveState
  final case class GetStats(chatId: Long)
  final case object GetArticles
  final case class AllArticles(articles: Iterable[HabrArticle])

}

class LibraryActor(config: LibraryActorConfig) extends Actor with ActorLogging {
  import LibraryActor._

  val savesDir = new SavesDir(config.savesDir)

  val chatData =
    savesDir.loadLast().map{ file =>
      log.info(s"load previous state from ${file.getAbsolutePath}")
      ChatData.load(file)
    }.getOrElse{
      log.info(s"previous save wasn't found, use empty")
      ChatData.empty()
    }

  implicit val executionContext: ExecutionContextExecutor = context.dispatcher

  var chatDataLastTime: Date = DateUtils.currentDate

  override def preStart(): Unit = {
    context.system.scheduler.schedule(config.stateSaveInterval, config.stateSaveInterval, self, SaveState)
  }

  override def receive: Receive = {
    case ChangeSettings(chatId: Long, cmd: String) =>
      def updateSettings(updater: FilterSettings => FilterSettings): Chat => Chat =
        chat => chat.copy(filterSettings = updater(chat.filterSettings))

      cmd match {
        case Command("/reset") =>
          chatData.updateChat(chatId)(_ => Chat.withDefaultSettings(chatId))
        case Command("/subscribe") =>
          chatData.updateChat(chatId) {
            updateSettings(settings => settings.copy(updateAsSoonAsPossible = true))
          }
          requestUpdates(chatId, sender)
        case Command("/unsubscribe") =>
          chatData.updateChat(chatId) {
            updateSettings(settings => settings.copy(updateAsSoonAsPossible = false))
          }
        case CommandStringDouble("/author", name, weight) =>
          chatData.updateChat(chatId) {
            updateSettings(s => s.copy(authorWeights = s.authorWeights.updated(name, weight)))
          }
        case CommandStringDouble("/tag", name, weight) =>
          chatData.updateChat(chatId) {
            updateSettings(s => s.copy(tagWeights = s.tagWeights.updated(name, weight)))
          }
        case CommandDouble("/rating", ratingThreshold) =>
          chatData.updateChat(chatId) {
            updateSettings(s => s.copy(ratingThreshold = ratingThreshold))
          }
        case _ =>
          sender ! Reply(chatId, s"unknown command: '$cmd'")
      }

    case GetSettings(chatId) =>
      sender ! Reply(chatId, chatData.getChat(chatId).getSettingsPrettify)
    case RequestUpdatesForTg =>
      processNewPostSending(sender)
    case UpdateArticles(articles) =>
      chatData.updateArticles(articles)
    case PostWasSentToTg(chatId, sentArticle) =>
      chatData.addSentArticle(chatId, sentArticle)
    case SaveState =>
      saveState()
    case RequestUpdates(chatId) =>
      requestUpdates(chatId, sender)
    case GetArticles =>
      sender ! AllArticles(chatData.articles.values.toVector)
    case GetStats(chatId) =>
      sender ! Reply(chatId, getStatsMsg)
  }

  private def saveState(): Unit = {
    val dest = savesDir.newSave(DateUtils.currentDate)
    dest.getParentFile.mkdirs()
    ChatData.save(chatData, dest)
  }

  private def getStatsMsg: String =
    s"""articles: <b>${chatData.articles.size}</b>
       |users: <b>${chatData.chats.size}</b>
       |subscribed users: <b>${chatData.chats.values.count(_.filterSettings.updateAsSoonAsPossible)}</b>
      """.stripMargin

  private def processNewPostSending(tgBot: ActorRef): Unit =
    for ((id, chat) <- chatData.chats if chat.filterSettings.updateAsSoonAsPossible) {
      chatData.getNewArticles(id).foreach(tgBot ! _)
      chatData.getSentArticleUpdates(id).foreach(tgBot ! _)
    }

  private def requestUpdates(chatId: Long, tgBot: ActorRef): Unit = {
    val updates = chatData.getNewArticles(chatId)

    updates.view.take(3).foreach(tgBot ! _)

    if (updates.isEmpty) {
      tgBot ! Reply(chatId, "no new articles :(")
    }
  }
}
