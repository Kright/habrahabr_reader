package com.github.kright.habrareader.actors

import java.net.{SocketException, UnknownHostException}

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import com.github.kright.habrareader.AppConfig.ArticlesUpdaterConfig
import com.github.kright.habrareader.actors.LibraryActor.{AllArticles, GetArticles, UpdateArticle}
import com.github.kright.habrareader.loaders.HabrArticlesDownloader
import com.github.kright.habrareader.models.HabrArticle
import com.github.kright.habrareader.utils.DateUtils

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}


object ArticlesUpdaterActor {
  def props(config: ArticlesUpdaterConfig, library: ActorRef): Props =
    Props(new ArticlesUpdaterActor(config, library))

  private final case object SearchNewArticles
}

class ArticlesUpdaterActor private(config: ArticlesUpdaterConfig, library: ActorRef) extends Actor with ActorLogging {

  import ArticlesUpdaterActor._

  override def preStart(): Unit = {
    context.system.scheduler.schedule(0.second, config.searchNewArticlesIntervalSeconds.seconds, self, SearchNewArticles)
    context.system.scheduler.schedule(0.second, config.articlesUpdateIntervalSeconds.seconds, library, GetArticles)
  }

  override def receive: Receive = {
    case SearchNewArticles => searchNewArticles()
    case AllArticles(articles) => updateOldestArticles(articles.toVector, 30, sender)
  }

  private def doSafe[T](func: => T, default: => T, handleException: Throwable => Unit): T =
    Try(func) match {
      case Success(result) =>
        result
      case Failure(exception) =>
        handleException(exception)
        default
    }

  private def logException(text: String, printTrace: Throwable => Boolean = _ => true): Throwable => Unit =
    ex => log.error(s"$text $ex ${if (printTrace(ex)) ex.getStackTrace.mkString("\n") else ""}")

  private def isInterestingException: Throwable => Boolean = {
    case _: UnknownHostException => false
    case _: SocketException => false
    case _ => true
  }

  def searchNewArticles(): Unit = {
    for {
      rssArticle <- doSafe(HabrArticlesDownloader.downloadRSSArticles, Seq(), logException("can't download rss articles", isInterestingException))
      article <- doSafe(Option(HabrArticlesDownloader.downloadArticle(rssArticle.link, rssArticle.publicationDate)), None, logException("can't download article", isInterestingException))
    } {
      log.info(s"add new article: ${article.link}")
      library ! UpdateArticle(article)
    }
  }

  def updateOldestArticles(articles: Vector[HabrArticle], maxCount: Int, library: ActorRef): Unit = Future {
    val threshold = DateUtils.now.getTime - 3.days.toMillis
    for {
      article <- articles.filter(_.publicationDate.getTime > threshold).sortBy(_.lastUpdateTime.getTime).take(maxCount)
      updatedArticle <- doSafe(Option(HabrArticlesDownloader.downloadArticle(article.link, article.publicationDate)), None, logException(s"can't update article ${article.link}", isInterestingException))
    } {
      library ! UpdateArticle(updatedArticle)
    }
  }
}
