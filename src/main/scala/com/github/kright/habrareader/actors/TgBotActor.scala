package com.github.kright.habrareader.actors

import java.net.{InetSocketAddress, Proxy}
import java.util.concurrent.Executors

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import cats.instances.future._
import cats.syntax.functor._
import com.bot4s.telegram.api.RequestHandler
import com.bot4s.telegram.api.declarative.Commands
import com.bot4s.telegram.future.{Polling, TelegramBot}
import com.bot4s.telegram.methods.{EditMessageText, ParseMode, SendMessage}
import com.bot4s.telegram.models.Message
import com.github.kright.habrareader.AppConfig.TgBotActorConfig
import com.github.kright.habrareader.actors.LibraryActor._
import com.github.kright.habrareader.models.{HabrArticle, SentArticle}
import com.github.kright.habrareader.utils.{ChangeSettings, CustomScalajHttpClient}
import com.github.kright.habrareader.utils.ChangeSettings._

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}


object TgBotActor {
  def props(config: TgBotActorConfig, library: ActorRef) = Props(new TgBotActor(config, library))

  final case class SendMessageToTg(chatId: Long, msg: String)

  final case class UpdateArticle(chatId: Long, article: HabrArticle, messageId: Option[Int])

}

class TgBotActor private(config: TgBotActorConfig, library: ActorRef) extends Actor with ActorLogging {

  import TgBotActor._

  private val threadsCount = 2
  private implicit val executionContext: ExecutionContext = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(threadsCount))

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
    case SendMessageToTg(chatId, msg) => sendMessageToTg(chatId, msg)
    case msg: UpdateArticle => updateArticle(msg, sender)
    case msg: RequestUpdates => library ! msg
    case msg: UpdateChat => library ! msg
    case msg: GetStats => library ! msg
    case msg: SaveState => library ! msg
    case unknownMessage => log.error(s"unknown message: $unknownMessage")
  }

  private def unsubscribeIfBotBanned(ex: Throwable, chatId: Long): Unit =
    ex match {
      case r: RuntimeException if r.getMessage == "Error 403 on request" =>
        log.error(s"can't send message to $chatId: $ex, '${ex.getMessage}', so unsubscribe this user")
        library ! UpdateChat(chatId,
          chat => chat.copy(filterSettings = chat.filterSettings.copy(updateAsSoonAsPossible = false)),
          isSilent = true
        )
      case _ =>
    }

  private def sendMessageToTg(chatId: Long, msg: String): Unit  =
    bot.request(SendMessage(chatId, msg, parseMode = Some(ParseMode.HTML))).onComplete {
      case Success(_) =>
      case Failure(ex) =>
        log.error(s"can't send message: $ex")
        unsubscribeIfBotBanned(ex, chatId)
    }

  private def updateArticle(update: UpdateArticle, sender: ActorRef): Unit = {
    update match {
      case UpdateArticle(chatId, article, None) =>
        bot.request(SendMessage(chatId, formMessage(article), parseMode = Some(ParseMode.HTML)))
          .onComplete {
            case Success(sentMsg) =>
              log.info(s"UpdateArticle($chatId, ${article.link})")
              sender ! PostWasSentToTg(chatId, SentArticle(sentMsg.messageId, article.id, article.lastUpdateTime))
            case Failure(ex) =>
              log.error(s"can't send article update ${article.link} for chat $chatId: $ex")
              unsubscribeIfBotBanned(ex, chatId)
          }
      case UpdateArticle(chatId, article, Some(messageId)) =>
        bot.request(EditMessageText(Option(chatId), Option(messageId), text = formMessage(article), parseMode = Some(ParseMode.HTML)))
          .onComplete {
            case Success(_) =>
              log.info(s"UpdateArticle($chatId, ${article.link}, $messageId)")
              sender ! PostWasSentToTg(chatId, SentArticle(messageId, article.id, article.lastUpdateTime))
            case Failure(ex) =>
              log.error(s"can't update existing message $messageId for chat $chatId: $ex")
              unsubscribeIfBotBanned(ex, chatId)
          }
    }
  }
}

class TgBot(override val client: RequestHandler[Future]) extends TelegramBot with Polling with Commands[Future]

class ObservableTgBot(override val client: RequestHandler[Future], observer: ActorRef, admins: Set[Long]) extends TgBot(client) {
  onCommand('settings) { msg =>
    Future {
      observer ! GetSettings(msg.chat.id)
    }
  }

  onCommand('reset | 'author | 'tag | 'company | 'rating | 'subscribe | 'unsubscribe) { msg =>
    Future {
      val cmd = msg.text.get
      val chatId = msg.chat.id

      val cmds = ChangeSettings.parse(cmd)
      if (cmds.nonEmpty) {
        observer ! UpdateChat(chatId, ChangeSettings.concatCommands(cmds))

        cmds.find {
          case ChangeSubscription(true) => true
          case _ => false
        }.foreach { _ =>
          observer ! RequestUpdates(msg.chat.id)
        }
      }
    }
  }

  onAdminCommand('save) { msg =>
    observer ! SaveState(Some(msg.chat.id))
  }

  onAdminCommand('stats) { msg =>
    observer ! GetStats(msg.chat.id)
  }

  onCommand('new) { msg =>
    Future {
      observer ! RequestUpdates(msg.chat.id)
    }
  }

  onCommand('start | 'help) { implicit msg =>
    reply(
      s"""Subscription to habrahabr updates with custom filtering
         |/start | /help - list commands
         |/new - request new articles
         |/subscribe - receive new articles as soon as possible
         |/unsubscribe - unsubscribe from receiving articles
         |/settings - print all settings
         |/reset - reset all weights to default
         |/author name weight
         |/tag name weight
         |/company name weight
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
    new ObservableTgBot(new CustomScalajHttpClient(botConfig.token, proxy), observer, admins)
  }
}
