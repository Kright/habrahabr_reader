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
import com.bot4s.telegram.models.Message
import com.github.kright.habrareader.AppConfig.TgBotActorConfig
import com.github.kright.habrareader.actors.LibraryActor._
import com.github.kright.habrareader.models.{Chat, FilterSettings, HabrArticle, SentArticle}
import com.github.kright.habrareader.utils.SettingsRequestParser.{Command, CommandDouble, CommandStringDouble}

import scala.concurrent.{ExecutionContext, Future}


object TgBotActor {
  def props(config: TgBotActorConfig, library: ActorRef) = Props(new TgBotActor(config, library))

  final case class SendMessageToTg(chatId: Long, msg: String)
  final case class UpdateArticle(chatId: Long, article: HabrArticle, messageId: Option[Int])
}

class TgBotActor private(config: TgBotActorConfig, library: ActorRef) extends Actor with ActorLogging {
  import TgBotActor._

  import ExecutionContext.Implicits.global

  private val bot = ObservableTgBot(config, self, config.admins)

  override def preStart(): Unit = {
    context.system.scheduler.schedule(config.chatsUpdateInterval, config.chatsUpdateInterval, library, RequestUpdatesForAll(config.updateExistingMessages))
    bot.run()
    config.admins.foreach { chatId =>
      self ! SendMessageToTg(chatId, "bot started!")
    }
  }

  private def formMessage(article: HabrArticle): String =
    article.metrics.map { m =>
      s"""author: <b>${article.author}</b>
         |rating: <b>${m.upVotes - m.downVotes}</b> = <b>${m.upVotes}</b> - <b>${m.downVotes}</b>
         |<b>${m.viewsCount}</b> views, <b>${m.bookmarksCount}</b> bookmarks, <b>${m.commentsCount}</b> comments
         |tags: ${article.categoriesNormalized.map(t => s"#$t").mkString("{", ", ", "}")}
         |${article.link}
      """.stripMargin
    }.getOrElse(s"author: ${article.author}")

  override def receive: Receive = {
    case GetSettings(chatId) => library ! LibraryActor.GetSettings(chatId)
    case SendMessageToTg(chatId, msg) =>
      val sent = bot.request(SendMessage(chatId, msg, parseMode = Some(ParseMode.HTML)))
      sent.failed.foreach { ex =>
        log.error(s"can't send message: $ex")
        if (ex.getMessage == "Error 403 on request") {
          log.error(s"can't send message to $chatId: $ex, '${ex.getMessage}', so unsubscribe this user")
          library ! UpdateChat(chatId, chat =>
            chat.copy(filterSettings = chat.filterSettings.copy(updateAsSoonAsPossible = false))
          )
        }
      }
    case UpdateArticle(chatId, article, None) =>
      val sent = bot.request(SendMessage(chatId, formMessage(article), parseMode = Some(ParseMode.HTML)))
      sent.failed.foreach { ex =>
        log.error(s"can't send info about new article $ex")
      }
      sent.map(msg => PostWasSentToTg(chatId, SentArticle(msg.messageId, article.id, article.lastUpdateTime))) // todo read about pipeTo
        .pipeTo(sender)
    case UpdateArticle(chatId, article, Some(messageId)) =>
      bot.request(EditMessageText(Option(chatId), Option(messageId), text = formMessage(article), parseMode = Some(ParseMode.HTML)))
        .failed.foreach(ex => log.error(s"can't update existing message $ex"))
    case msg: RequestUpdates => library ! msg
    case msg: UpdateChat => library ! msg
    case msg: GetStats => library ! msg
    case msg: SaveState => library ! msg
    case unknownMessage => log.error(s"unknown message: $unknownMessage")
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
      val cmd = msg.text.get
      val chatId = msg.chat.id

      def updateSettings(updater: FilterSettings => FilterSettings): Chat => Chat =
        chat => chat.copy(filterSettings = updater(chat.filterSettings))

      def updateWeight(w: Map[String, Double], key: String, value: Double): Map[String, Double] =
        if (value == 0.0)
          w - key
        else
          w.updated(key, value)

      cmd match {
        case Command("/reset") =>
          observer ! UpdateChat(chatId, _ => Chat.withDefaultSettings(chatId))
        case Command("/subscribe") =>
          observer ! UpdateChat(chatId, updateSettings(settings => settings.copy(updateAsSoonAsPossible = true)))
          observer ! RequestUpdates(msg.chat.id)
        case Command("/unsubscribe") =>
          observer ! UpdateChat(chatId, updateSettings(settings => settings.copy(updateAsSoonAsPossible = false)))
        case CommandStringDouble("/author", name, weight) =>
          observer ! UpdateChat(chatId, updateSettings(s => s.copy(authorWeights = updateWeight(s.authorWeights, name, weight))))
        case CommandStringDouble("/tag", name, weight) =>
          observer ! UpdateChat(chatId, updateSettings(s => s.copy(tagWeights = updateWeight(s.tagWeights, name, weight))))
        case CommandDouble("/rating", ratingThreshold) =>
          observer ! UpdateChat(chatId, updateSettings(s => s.copy(ratingThreshold = ratingThreshold)))
        case _ =>
          observer ! SendMessageToTg(chatId, s"unknown command: '$cmd'")
      }
    }
  }

  onAdminCommand('save) { msg =>
    observer ! SaveState(msg.chat.id)
  }

  onAdminCommand('stats) { msg =>
    observer ! GetStats(msg.chat.id)
  }

  onAdminCommand('new) { msg =>
    observer ! RequestUpdates(msg.chat.id)
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
         |example: '/tag scala +10'
         |/rating thresholdValue
         |
         |repo: https://github.com/Kright/habrahabr_reader
      """.stripMargin).void
  }

  def onAdminCommand(s: Symbol)(action: Message => Unit): Unit =
    onCommand(s) { msg =>
      Future {
        if (admins.contains(msg.chat.id)) {
          action(msg)
        }
      }
    }
}

object ObservableTgBot {
  def apply(botConfig: TgBotActorConfig, observer: ActorRef, admins: Set[Long])(implicit ec: ExecutionContext): ObservableTgBot = {
    val proxy = if (botConfig.proxy.ip.isEmpty) Proxy.NO_PROXY else
      new Proxy(Proxy.Type.SOCKS, InetSocketAddress.createUnresolved(botConfig.proxy.ip, botConfig.proxy.port))
    new ObservableTgBot(new ScalajHttpClient(botConfig.token, proxy), observer, admins)
  }
}
