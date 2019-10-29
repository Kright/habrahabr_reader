package com.github.kright.habrareader.actors

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import com.github.kright.habrareader.AppConfig.ArticlesUpdaterConfig
import com.github.kright.habrareader.actors.LibraryActor.{AllArticles, GetArticles, UpdateArticles}
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

  def searchNewArticles(): Unit = {
    val now = DateUtils.now
    val habrArticles = HabrArticlesDownloader.getArticles()

    log.debug(s"add new articles: ${habrArticles.map(_.title).mkString("[", ", ", "]")}")
    library ! LibraryActor.UpdateArticles(habrArticles)
  }

  def updateOldestArticles(articles: Vector[HabrArticle], maxCount: Int, library: ActorRef): Unit = Future {
    val threshold = DateUtils.now.getTime - 3.days.toMillis
    val oldest = articles.filter(_.publicationDate.getTime > threshold).sortBy(_.lastUpdateTime.getTime).take(maxCount)

    oldest.foreach { article =>
      Try(HabrArticlesDownloader.downloadArticle(article.link, article.publicationDate)) match {
        case Failure(ex) =>
          log.error(s"can't download article ${article.link}: ${ex} ${ex.getStackTrace.mkString("\n")}")
        case Success(updatedArticle) =>
          log.info(s"article updated: ${updatedArticle.link}")
          library ! UpdateArticles(Seq(updatedArticle))
      }
    }
  }
}
