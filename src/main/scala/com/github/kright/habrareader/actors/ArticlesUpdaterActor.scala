package com.github.kright.habrareader.actors

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import akka.pattern.pipe
import com.github.kright.habrareader.AppConfig.ArticlesUpdaterConfig
import com.github.kright.habrareader.actors.LibraryActor.{AllArticles, GetArticles}
import com.github.kright.habrareader.loaders.HabrArticlesDownloader
import com.github.kright.habrareader.models.HabrArticle
import com.github.kright.habrareader.utils.DateUtils

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration._


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
    case AllArticles(articles) => updateOldestArticles(articles.toVector, 30)
  }

  def searchNewArticles(): Unit = {
    val now = DateUtils.currentDate
    val habrArticles = HabrArticlesDownloader.getArticles()

    log.debug(s"add new articles: ${habrArticles.map(_.title).mkString("[", ", ", "]")}")
    library ! LibraryActor.UpdateArticles(habrArticles)
  }

  def updateOldestArticles(articles: Vector[HabrArticle], maxCount: Int): Unit = {
    val threshold = DateUtils.currentDate.getTime - 3.days.toMillis

    val oldest = articles.filter(_.lastUpdateTime.getTime > threshold).sortBy(_.lastUpdateTime.getTime).take(maxCount)

    oldest.foreach { article =>
      Future {
        val updated = HabrArticlesDownloader.downloadArticle(article.link, article.publicationDate)
        LibraryActor.UpdateArticles(List(updated))
      } pipeTo library
    }
  }
}
