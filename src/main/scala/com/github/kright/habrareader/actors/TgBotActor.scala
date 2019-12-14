package com.github.kright.habrareader.actors

import java.util.concurrent.Executors

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import akka.pattern.{ask, pipe}
import akka.util.Timeout
import com.bot4s.telegram.methods.{EditMessageText, ParseMode, SendMessage}
import com.bot4s.telegram.models.Message
import com.github.kright.habrareader.AppConfig.TgBotActorConfig
import com.github.kright.habrareader.actors.LibraryActor._
import com.github.kright.habrareader.models.{HabrArticle, SentArticle}
import com.github.kright.habrareader.utils.ChangeSettings

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import scala.util.{Failure, Success}


object TgBotActor {
  def props(config: TgBotActorConfig, library: ActorRef) = Props(new TgBotActor(config, library))

  final case class SendMessageToTg(chatId: Long, msg: String)

  final case class UpdateArticle(chatId: Long, article: HabrArticle, messageId: Option[Int])

  final case class ReceiveTgMessage(message: Message)

}

class TgBotActor private(config: TgBotActorConfig, library: ActorRef) extends Actor with ActorLogging {

  import TgBotActor._

  private val threadsCount = 2
  private implicit val executionContext: ExecutionContext = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(threadsCount))

  private val bot = TgBotAdapter(config, self)
  private val admins = config.admins

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
    case SendMessageToTg(chatId, msg) => sendMessageToTg(chatId, msg)
    case msg: UpdateArticle => updateArticle(msg, sender)
    case ReceiveTgMessage(msg) => processTgMessage(msg)
    case unknownMessage => log.error(s"unknown message: $unknownMessage")
  }

  private def getFirstToken(text: String): Option[String] =
    text.trim.split(" ").headOption.map(_.toLowerCase.stripPrefix("/"))

  private def processTgMessage(implicit message: Message): Unit = {
    val firstToken = message.text.flatMap(getFirstToken).getOrElse("")
    val chatId = message.chat.id
    val isAdmin = admins.contains(message.chat.id)

    firstToken match {
      case "settings" => library ! GetSettings(chatId)
      case "new" => library ! RequestUpdates(chatId)
      case "start" | "help" => sendMessageToTg(chatId, helpMsgReplyText)
      case "reset" | "author" | "tag" | "company" | "rating" | "subscribe" | "unsubscribe" =>
        processChangeSettingsCmd(chatId, message.text.get)
      case "save" if isAdmin => save(message.chat.id)
      case "stats" if isAdmin => library ! GetStats(chatId)
      case "stop" if isAdmin => library ! saveAndStop(chatId)
      case _ => log.info(s"unknown text: '${message.text.getOrElse("")}'")
    }
  }

  private def save(chatId: Long): Unit = {
    implicit val timeout: Timeout = 10.seconds
    (library ? SaveState(needConfirmation = true)) map { case Ok => SendMessageToTg(chatId, "saved!") } pipeTo self
  }

  private def saveAndStop(chatId: Long): Unit = {
    implicit val timeout: Timeout = 10.seconds
    (library ? SaveState(needConfirmation = true)) foreach { case Ok =>
      log.error("stop bot because of /stop admin command!")
      self ! SendMessageToTg(chatId, "saved!, stopping...")
      context.system.scheduler.scheduleOnce(5.seconds) {
        sys.exit(0) // this isn't a best way to stop
      }
    }
  }

  private def processChangeSettingsCmd(chatId: Long, cmd: String): Unit = {
    implicit val timeout: Timeout = 10.seconds
    val cmds = ChangeSettings.parse(cmd)
    if (cmds.nonEmpty) {
      val upd = UpdateChat(chatId, ChangeSettings.concatCommands(cmds), needConfirmation = true)
      (library ? upd) map { case Ok => SendMessageToTg(chatId, "ok!") } pipeTo self
    } else {
      self ! SendMessageToTg(chatId, "can't parse commands")
    }
  }

  private def helpMsgReplyText: String =
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
       |repo: https://github.com/Kright/habrahabr_reader""".stripMargin

  private def unsubscribeIfBotBanned(ex: Throwable, chatId: Long): Unit =
    ex match {
      case r: RuntimeException if r.getMessage == "Error 403 on request" =>
        log.error(s"can't send message to $chatId: $ex, '${ex.getMessage}', so unsubscribe this user")
        library ! UpdateChat(chatId,
          chat => chat.copy(filterSettings = chat.filterSettings.copy(updateAsSoonAsPossible = false)),
          needConfirmation = false
        )
      case _ =>
    }

  private def sendMessageToTg(chatId: Long, msg: String): Unit =
    bot.request(SendMessage(chatId, msg, parseMode = Some(ParseMode.HTML))).onComplete {
      case Success(_) =>
      case Failure(ex) =>
        log.error(s"can't send message: $ex")
        unsubscribeIfBotBanned(ex, chatId)
    }

  private def updateArticle(update: UpdateArticle, sender: ActorRef): Unit =
    (update match {
      case UpdateArticle(chatId, article, None) =>
        bot.request(SendMessage(chatId, formMessage(article), parseMode = Some(ParseMode.HTML))).map(_.messageId)
      case UpdateArticle(chatId, article, Some(messageId)) =>
        bot.request(EditMessageText(Option(chatId), Option(messageId), text = formMessage(article), parseMode = Some(ParseMode.HTML))).map(_ => messageId)
    }).onComplete {
      case Success(sentMessageId) =>
        log.info(s"$update")
        sender ! PostWasSentToTg(update.chatId, SentArticle(sentMessageId, update.article.id, update.article.lastUpdateTime))
      case Failure(ex) =>
        log.error(s"can't send article update $update: $ex")
        unsubscribeIfBotBanned(ex, update.chatId)
    }
}
