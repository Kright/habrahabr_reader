package com.github.awant.habrareader.actors

import java.net.{InetSocketAddress, Proxy}

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import com.bot4s.telegram.api.RequestHandler
import com.bot4s.telegram.api.declarative.Commands
import com.bot4s.telegram.clients.ScalajHttpClient
import com.bot4s.telegram.future.{Polling, TelegramBot}
import com.bot4s.telegram.methods.{EditMessageText, ParseMode, SendMessage}
import com.github.awant.habrareader.models.{Event, Post}
import slogging.{LogLevel, LoggerConfig, PrintLoggerFactory}
import cats.instances.future._
import cats.syntax.functor._
import com.github.awant.habrareader.AppConfig.TgBotActorConfig
import com.github.awant.habrareader.actors.LibraryActor.PostWasSentToTg
import akka.pattern.pipe

import scala.concurrent.{ExecutionContext, Future}


object TgBotActor {
  def props(config: TgBotActorConfig, library: ActorRef) = Props(new TgBotActor(config, library))

  final case class Subscription(chatId: Long, set: Boolean)
  final case class Settings(chatId: Long)
  final case class SettingsUpd(chatId: Long, text: String)
  final case class Reply(chatId: Long, msg: String)
  final case class PostReply(chatId: Long, post: Post)
  final case class PostEdit(chatId: Long, messageId: Int, post: Post)
}

class TgBotActor private(botConfig: TgBotActorConfig, library: ActorRef) extends Actor with ActorLogging {
  import TgBotActor._

  import ExecutionContext.Implicits.global

  private val bot = ObservableTgBot(botConfig, self)

  override def preStart(): Unit = {
    library ! LibraryActor.BotSubscription(self)
    bot.run()
  }

  private def formMessage(post: Post): String = {
    s"""author: ${post.author}
         |up votes: ${post.upVotes}
         |down votes: ${post.downVotes}
         |${post.viewsCount} views, ${post.bookmarksCount} bookmarks, ${post.commentsCount} comments
         |${post.link}
      """.stripMargin
  }

  override def receive: Receive = {
    case Subscription(chatId, set) => library ! LibraryActor.SubscriptionChanging(chatId, set)
    case Settings(chatId) => library ! LibraryActor.SettingsGetting(chatId)
    case SettingsUpd(chatId, body) => library ! LibraryActor.SettingsChanging(chatId, body)
    case Reply(chatId, msg) => bot.request(SendMessage(chatId, msg))
    case PostReply(chatId, post) =>
      bot.request(SendMessage(chatId, formMessage(post)))
        .map(msg => PostWasSentToTg(Event( chatId, msg.messageId, post.id, post.updateDate)))
        .pipeTo(sender)
    case PostEdit(chatId, messageId, post) =>
      bot.request(EditMessageText(Option(chatId), Option(messageId), text=formMessage(post)))
  }
}

class TgBot(override val client: RequestHandler[Future]) extends TelegramBot with Polling with Commands[Future]

class ObservableTgBot(override val client: RequestHandler[Future], observer: ActorRef) extends TgBot(client) {
  import TgBotActor._

  onCommand('subscribe) { msg =>
    Future { observer ! Subscription(msg.chat.id, set=true) }
  }

  onCommand('unsubscribe) { msg =>
    Future { observer ! Subscription(msg.chat.id, set=false) }
  }

  onCommand('settings) { msg =>
    Future { observer ! Settings(msg.chat.id) }
  }

  onCommand('reset | 'author | 'tag | 'rating) { msg =>
    Future { observer ! SettingsUpd(msg.chat.id, msg.text.get) }
  }

  onCommand('start | 'help) { implicit msg =>
    reply(
      s"""Subscription to habrahabr updates with custom filtering
         |/start | /help - list commands
         |/subscribe - subscribe to receive new articles
         |/unsubscribe - unsubscribe
         |/settings - print all settings
         |/reset - reset all weights to default
         |/author name weight
         |/tag name weight
         |/rating thresholdValue
         |
         |example: '/tag scala +10'
      """.stripMargin, Option(ParseMode.Markdown)).void
  }
}

object ObservableTgBot {
  LoggerConfig.factory = PrintLoggerFactory()
  LoggerConfig.level = LogLevel.TRACE

  def apply(botConfig: TgBotActorConfig, observer: ActorRef)(implicit ec: ExecutionContext): ObservableTgBot = {
    val proxy = if (botConfig.proxy.ip.isEmpty) Proxy.NO_PROXY else
      new Proxy(Proxy.Type.SOCKS, InetSocketAddress.createUnresolved(botConfig.proxy.ip, botConfig.proxy.port))
    new ObservableTgBot(new ScalajHttpClient(botConfig.token, proxy), observer)
  }
}
