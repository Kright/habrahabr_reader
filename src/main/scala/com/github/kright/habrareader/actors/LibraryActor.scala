package com.github.kright.habrareader.actors

import java.util.Date

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import com.github.kright.habrareader.AppConfig.LibraryActorConfig
import com.github.kright.habrareader.actors.TgBotActor.{ArticleEdit, ArticleReply, Reply}
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
  final case class UpdateArticles(posts: Seq[HabrArticle])
  final case object SaveState
}

class LibraryActor(config: LibraryActorConfig) extends Actor with ActorLogging {
  // todo test loading data from jsons
  import LibraryActor._

  val savesDir = new SavesDir(config.savesDir)

  // todo print logs if can't load
  // todo handling errors
  val chatData = savesDir.loadLast().map(ChatData.load).getOrElse(ChatData.empty())

  implicit val executionContext: ExecutionContextExecutor = context.dispatcher

  var chatDataLastTime: Date = DateUtils.currentDate

  override def preStart(): Unit = {
    context.system.scheduler.schedule(config.stateSaveInterval, config.stateSaveInterval, self, SaveState)
  }

  override def receive: Receive = {
    case ChangeSettings(chatId: Long, cmd: String) =>
      println(s"SettingsChanging($chatId, $cmd)") // todo use logs for this

      def updateSettings(updater: FilterSettings => FilterSettings): Chat => Chat =
        chat => chat.copy(filterSettings = updater(chat.filterSettings))

      cmd match {
        case Command("/reset") =>
          chatData.updateChat(chatId)(_ => Chat.withDefaultSettings(chatId))
        case Command("/subscribe") =>
          chatData.updateChat(chatId) {
            updateSettings(settings => settings.copy(updateAsSoonAsPossible = true))
          }
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
    case UpdateArticles(posts) =>
      chatData.updatePosts(posts)
    case PostWasSentToTg(chatId, sentArticle) =>
      chatData.addSentArticle(chatId, sentArticle)
    case SaveState =>
      saveState()
    case RequestUpdates(chatId) =>
      requestUpdates(chatId, sender)
  }

  private def saveState(): Unit = {
    val dest = savesDir.newSave(DateUtils.currentDate)
    dest.getParentFile.mkdirs()
    ChatData.save(chatData, dest)
  }

  private def processNewPostSending(tgBot: ActorRef): Unit = {
    val currentLast = chatDataLastTime

    val updates = chatData.getUpdates(currentLast)
    val newLastDate = updates.view.map(_.date).foldLeft(currentLast)(DateUtils.getLast)
    chatDataLastTime = newLastDate

    updates.foreach {
      case ChatData.Update(chat, post, None) =>
        tgBot ! ArticleReply(chat.id, post)
      case ChatData.Update(chat, post, Some(prevMessageId)) =>
        tgBot ! ArticleEdit(chat.id, prevMessageId, post)
    }
  }

  private def requestUpdates(chatId: Long, tgBot: ActorRef): Unit = {
    val updates = chatData.getNewArticlesForChat(chatId).view.take(3)

    updates.foreach {
      case ChatData.Update(chat, post, None) =>
        tgBot ! ArticleReply(chat.id, post)
      case ChatData.Update(chat, post, Some(prevMessageId)) =>
        tgBot ! ArticleEdit(chat.id, prevMessageId, post)
    }

    if (updates.isEmpty) {
      tgBot ! Reply(chatId, "no new articles :(")
    }
  }
}