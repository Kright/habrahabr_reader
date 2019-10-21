package com.github.kright.habrareader.actors

import java.net.{InetSocketAddress, Proxy}

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import akka.pattern.pipe
import cats.instances.future._
import cats.syntax.functor._
import com.bot4s.telegram.api.RequestHandler
import com.bot4s.telegram.api.declarative.Commands
import com.bot4s.telegram.clients.ScalajHttpClient
import com.bot4s.telegram.future.{Polling, TelegramBot}
import com.bot4s.telegram.methods.{EditMessageText, ParseMode, SendMessage}
import com.github.kright.habrareader.AppConfig.TgBotActorConfig
import LibraryActor.{PostWasSentToTg, RequestUpdates, RequestUpdatesForTg, SaveState}
import com.github.kright.habrareader.models.{HabrArticle, SentArticle}
import slogging.{LogLevel, LoggerConfig, PrintLoggerFactory}

import scala.concurrent.{ExecutionContext, Future}


object TgBotActor {
  def props(config: TgBotActorConfig, library: ActorRef) = Props(new TgBotActor(config, library))

  final case class GetSettings(chatId: Long)
  final case class SettingsUpd(chatId: Long, text: String)
  final case class Reply(chatId: Long, msg: String)
  final case class ArticleReply(chatId: Long, post: HabrArticle)
  final case class ArticleEdit(chatId: Long, messageId: Int, post: HabrArticle)
}

class TgBotActor private(config: TgBotActorConfig, library: ActorRef) extends Actor with ActorLogging {
  import TgBotActor._

  import ExecutionContext.Implicits.global

  private val bot = ObservableTgBot(config, self, config.admins)

  override def preStart(): Unit = {
    context.system.scheduler.schedule(config.chatsUpdateInterval, config.chatsUpdateInterval, self, RequestUpdatesForTg)
    bot.run()
  }

  private def formMessage(article: HabrArticle): String =
    article.metrics.map { m =>
      s"""author: *${article.author}*
         |rating: *${m.upVotes - m.downVotes}* = *${m.upVotes}* - *${m.downVotes}*
         |*${m.viewsCount}* views, *${m.bookmarksCount}* bookmarks, *${m.commentsCount}* comments
         |${article.link}
      """.stripMargin
    }.getOrElse(s"author: ${article.author}")

  override def receive: Receive = {
    case GetSettings(chatId) => library ! LibraryActor.GetSettings(chatId)
    case SettingsUpd(chatId, body) => library ! LibraryActor.ChangeSettings(chatId, body)
    case Reply(chatId, msg) => bot.request(SendMessage(chatId, msg))
    case ArticleReply(chatId, post) =>
      bot.request(SendMessage(chatId, formMessage(post), parseMode = Some(ParseMode.Markdown)))
        .map(msg => PostWasSentToTg(chatId, SentArticle(msg.messageId, post.id, post.lastUpdateTime)))
        .pipeTo(sender)
    case ArticleEdit(chatId, messageId, post) =>
      bot.request(EditMessageText(Option(chatId), Option(messageId), text = formMessage(post)))
    case SaveState =>
      library ! SaveState
    case r: RequestUpdates =>
      library ! r
  }
}

class TgBot(override val client: RequestHandler[Future]) extends TelegramBot with Polling with Commands[Future]

class ObservableTgBot(override val client: RequestHandler[Future], observer: ActorRef, admins: Set[Long]) extends TgBot(client) {
  import TgBotActor._

  onCommand('settings) { msg =>
    Future {
      observer ! GetSettings(msg.chat.id)
    }
  }

  onCommand('reset | 'author | 'tag | 'rating | 'subscribe | 'unsubscribe) { msg =>
    Future {
      observer ! SettingsUpd(msg.chat.id, msg.text.get)
    }
  }

  onCommand('new) { msg =>
    Future {
      observer ! RequestUpdates(msg.chat.id)
    }
  }

  onCommand('save) { msg =>
    Future {
      if (admins.contains(msg.chat.id)) {
        observer ! SaveState
      }
    }
  }

  onCommand('start | 'help) { implicit msg =>
    reply(
      s"""Subscription to habrahabr updates with custom filtering
         |/start | /help - list commands
         |/new - request new articles
         |/subscribe - receive new articles as soon as possible
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

  def apply(botConfig: TgBotActorConfig, observer: ActorRef, admins: Set[Long])(implicit ec: ExecutionContext): ObservableTgBot = {
    val proxy = if (botConfig.proxy.ip.isEmpty) Proxy.NO_PROXY else
      new Proxy(Proxy.Type.SOCKS, InetSocketAddress.createUnresolved(botConfig.proxy.ip, botConfig.proxy.port))
    new ObservableTgBot(new ScalajHttpClient(botConfig.token, proxy), observer, admins)
  }
}