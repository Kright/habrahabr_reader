package com.github.awant.habrareader.actors

import java.util.Date

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import com.github.awant.habrareader.AppConfig.LibraryActorConfig
import com.github.awant.habrareader.actors.TgBotActor.{PostEdit, PostReply, Reply}
import com.github.awant.habrareader.models.{Chat, ChatData, HabrArticle, SentArticle}
import com.github.awant.habrareader.utils.SettingsRequestParser._
import com.github.awant.habrareader.utils.{DateUtils, SavesDir}

import scala.concurrent.ExecutionContextExecutor


object LibraryActor {
  def props(config: LibraryActorConfig): Props = Props(new LibraryActor(config))

  final case class PostWasSentToTg(chatId: Long, sentArticle: SentArticle)
  final case class ChangeSubscription(chatId: Long, subscribe: Boolean)
  final case class GetSettings(chatId: Long)
  final case class ChangeSettings(chatId: Long, body: String)
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
    case ChangeSubscription(chatId: Long, subscribe: Boolean) =>
      chatData.updateChat(chatId) { chat =>
        chat.copy(subscription = subscribe)
      }

    case ChangeSettings(chatId: Long, cmd: String) =>
      println(s"SettingsChanging($chatId, $cmd)") // todo use logs for this

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
  }

  private def saveState(): Unit = {
    val dest = savesDir.newSave(DateUtils.currentDate)
    dest.mkdirs()
    ChatData.save(chatData, dest)
  }

  private def processNewPostSending(tgBot: ActorRef): Unit = {
    val currentLast = chatDataLastTime

    val updates = chatData.getUpdates(currentLast)
    val newLastDate = updates.view.map(_.date).foldLeft(currentLast)(DateUtils.getLast)
    chatDataLastTime = newLastDate

    updates.foreach {
      case ChatData.Update(chat, post, None) =>
        tgBot ! PostReply(chat.id, post)
      case ChatData.Update(chat, post, Some(prevMessageId)) =>
        tgBot ! PostEdit(chat.id, prevMessageId, post)
    }
  }
}
