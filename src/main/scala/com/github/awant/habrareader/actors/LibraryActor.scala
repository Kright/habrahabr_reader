package com.github.awant.habrareader.actors

import java.util.Date

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import com.github.awant.habrareader.AppConfig.LibraryActorConfig
import com.github.awant.habrareader.actors.TgBotActor.{PostEdit, PostReply, Reply}
import com.github.awant.habrareader.models.{Chat, ChatData, HabrArticle, SentArticle}
import com.github.awant.habrareader.utils.SettingsRequestParser._
import com.github.awant.habrareader.utils.{DateUtils, SavesDir}

import scala.concurrent.ExecutionContextExecutor
import scala.concurrent.duration._


object LibraryActor {
  def props(config: LibraryActorConfig): Props = Props(new LibraryActor(config))

  final case class BotSubscription(subscriber: ActorRef)
  final case class PostWasSentToTg(chatId: Long, sentArticle: SentArticle)
  final case class ChangeSubscription(chatId: Long, subscribe: Boolean)
  final case class GetSettings(chatId: Long)
  final case class ChangeSettings(chatId: Long, body: String)
  final case object NewPostsSending
  final case class UpdateArticles(posts: Seq[HabrArticle])
  final case object SaveState
}

class LibraryActor(config: LibraryActorConfig) extends Actor with ActorLogging {
  // todo test leoading data from jsons
  import LibraryActor._

  val savesDir = new SavesDir(config.savesDir)

  // todo print logs if can't load
  // todo handling errors
  val chatData = savesDir.loadLast().map(ChatData.load).getOrElse(ChatData.empty())

  implicit val executionContext: ExecutionContextExecutor = context.dispatcher

  // todo refactor this, don't store ref
  var subscribedBot: ActorRef = _
  var chatDataLastTime: Date = DateUtils.currentDate

  override def preStart(): Unit = {
    context.system.scheduler.schedule(config.chatsUpdateInterval, config.chatsUpdateInterval, self, NewPostsSending)
    context.system.scheduler.schedule(config.stateSaveInterval, config.stateSaveInterval, self, SaveState)
  }

  override def receive: Receive = {
    // todo rm this
    case BotSubscription(subscriber) => subscribedBot = subscriber

    case ChangeSubscription(chatId: Long, subscribe: Boolean) =>
      chatData.updateChat(chatId) { chat =>
        chat.copy(subscription = subscribe)
      }

    case ChangeSettings(chatId: Long, cmd: String) =>
      println(s"SettingsChanging($chatId, $cmd)")

      cmd match {
        case Command("/reset") =>
          chatData.updateChat(chatId)(_ => Chat.withDefaultSettings(chatId))
        case CommandStringDouble("/author", name, weight) =>
          chatData.updateChat(chatId) { chat =>
            chat.copy(authorWeights = chat.authorWeights.updated(name, weight))
          }
        case CommandStringDouble("/tag", name, weight) =>
          chatData.updateChat(chatId) { chat =>
            chat.copy(tagWeights = chat.tagWeights.updated(name, weight))
          }
        case CommandDouble("/rating", ratingThreshold) =>
          chatData.updateChat(chatId) {
            _.copy(ratingThreshold = ratingThreshold)
          }
        case _ =>
          subscribedBot ! Reply(chatId, s"unknown command: '$cmd'")
      }

    case GetSettings(chatId) =>
      subscribedBot ! Reply(chatId, chatData.getChat(chatId).getSettingsPrettify)
    case NewPostsSending =>
      processNewPostSending()
    case UpdateArticles(posts) =>
      chatData.updatePosts(posts)
    case PostWasSentToTg(chatId, sentArticle) =>
      chatData.addSentArticle(chatId, sentArticle)
    case SaveState =>
      saveState()
  }

  private def saveState(): Unit = {
    val dest = savesDir.newSave(DateUtils.currentDate)
    dest.mkdirs()
    ChatData.save(chatData, dest)
  }

  private def processNewPostSending(): Unit = {
    val currentLast = chatDataLastTime

    val updates = chatData.getUpdates(currentLast)
    val newLastDate = updates.view.map(_.date).foldLeft(currentLast)(DateUtils.getLast)
    chatDataLastTime = newLastDate

    updates.foreach {
      case ChatData.Update(chat, post, None) =>
        subscribedBot ! PostReply(chat.id, post)
      case ChatData.Update(chat, post, Some(prevMessageId)) =>
        subscribedBot ! PostEdit(chat.id, prevMessageId, post)
    }
  }
}
