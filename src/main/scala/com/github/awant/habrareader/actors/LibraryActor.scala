package com.github.awant.habrareader.actors

import java.util.Date

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import com.github.awant.habrareader.AppConfig.LibraryActorConfig
import com.github.awant.habrareader.actors.TgBotActor.{PostEdit, PostReply, Reply}
import com.github.awant.habrareader.models.SentArticle
import com.github.awant.habrareader.models.{Chat, ChatData, HabrArticle}
import com.github.awant.habrareader.utils.{DateUtils, SavesDir}
import com.github.awant.habrareader.utils.SettingsRequestParser._

import scala.concurrent.ExecutionContextExecutor
import scala.concurrent.duration._


object LibraryActor {
  def props(config: LibraryActorConfig): Props = Props(new LibraryActor(config))

  final case class BotSubscription(subscriber: ActorRef)
  final case class PostWasSentToTg(chatId: Long, sentArticle: SentArticle)

  final case class SubscriptionChanging(chatId: Long, subscribe: Boolean)
  final case class SettingsGetting(chatId: Long)
  final case class SettingsChanging(chatId: Long, body: String)
  final case object NewPostsSending
  final case class PostsUpdating(posts: Seq[HabrArticle])
}

class LibraryActor(config: LibraryActorConfig) extends Actor with ActorLogging {
  // todo test leoading data from jsons
  import LibraryActor._

  val subscriptionReplyInterval: FiniteDuration = config.chatsUpdateTimeSeconds.seconds
  val savesDir = new SavesDir(config.savesDir)

  // todo print logs if can't load
  // todo handling errors
  val chatData = savesDir.loadLast().map(ChatData.load).getOrElse(ChatData.empty())

  implicit val executionContext: ExecutionContextExecutor = context.dispatcher

  // todo refactor this, don't store ref
  var subscribedBot: ActorRef = _
  var chatDataLastTime: Date = DateUtils.currentDate

  // todo add saving state

  override def preStart(): Unit = {
    context.system.scheduler.schedule(subscriptionReplyInterval, subscriptionReplyInterval, self, NewPostsSending)
  }

  override def receive: Receive = {
    case BotSubscription(subscriber) => subscribedBot = subscriber

    case SubscriptionChanging(chatId: Long, subscribe: Boolean) =>
      chatData.updateChat(chatId) { chat =>
        chat.copy(subscription = subscribe)
      }

    case SettingsChanging(chatId: Long, cmd: String) =>
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

    case SettingsGetting(chatId) =>
      subscribedBot ! Reply(chatId, chatData.getChat(chatId).getSettingsPrettify)
    case NewPostsSending =>
      processNewPostSending()
    case PostsUpdating(posts) =>
      chatData.updatePosts(posts)
    case PostWasSentToTg(chatId, sentArticle) =>
      chatData.addSentArticle(chatId, sentArticle)
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
