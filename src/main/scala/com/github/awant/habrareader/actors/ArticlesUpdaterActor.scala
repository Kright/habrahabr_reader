package com.github.awant.habrareader.actors

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import com.github.awant.habrareader.AppConfig.ArticlesUpdaterConfig
import com.github.awant.habrareader.loaders.HabrArticlesDownloader
import com.github.awant.habrareader.utils.DateUtils

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._


object ArticlesUpdaterActor {
  def props(config: ArticlesUpdaterConfig, library: ActorRef): Props =
    Props(new ArticlesUpdaterActor(config, library))

  final case class UpdatePosts()
}

class ArticlesUpdaterActor private(config: ArticlesUpdaterConfig, library: ActorRef) extends Actor with ActorLogging {

  import ArticlesUpdaterActor._

  override def preStart(): Unit = {
    context.system.scheduler.schedule(0.second, config.articlesUpdateInterval, self, UpdatePosts)
  }

  override def receive: Receive = {
    case UpdatePosts => updatePosts()
  }

  def updatePosts(): Unit = {
    val now = DateUtils.currentDate

    val habrArticles = HabrArticlesDownloader.getArticles()

    log.debug(s"update posts: ${habrArticles.map(_.title).mkString("[", ", ", "]")}")

    library ! LibraryActor.UpdateArticles(habrArticles)
  }
}
