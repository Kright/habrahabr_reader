package com.github.kright.habrareader.models

import java.util.Date

import com.github.kright.habrareader.utils.DateUtils._
import io.circe.syntax._
import io.circe.{Decoder, Encoder, HCursor, Json}

case class HabrArticle(id: Int,
                       link: String,
                       title: String,
                       description: String,
                       author: String,
                       categories: Set[String],
                       metrics: Option[ArticleMetrics],
                       publicationDate: Date,
                       lastUpdateTime: Date) {
  override def hashCode(): Int = id

  override def equals(obj: Any): Boolean =
    obj match {
      case art: HabrArticle => art.id == this.id
      case _ => false
    }
}

object HabrArticle {
  implicit val encoder: Encoder[HabrArticle] = (article: HabrArticle) => {
    Json.obj(
      "id" := article.id,
      "link" := article.link,
      "title" := article.title,
      "description" := article.description,
      "author" := article.author,
      "categories" := article.categories,
      "publicationDate" := article.publicationDate,
      "lastUpdateTime" := article.lastUpdateTime,
      "categories" := article.categories,
      "metrics" := article.metrics,
    )
  }

  implicit val decoder: Decoder[HabrArticle] = (c: HCursor) => {
    for {
      id <- c.get[Int]("id")
      link <- c.get[String]("link")
      title <- c.get[String]("title")
      description <- c.get[String]("description")
      author <- c.get[String]("author")
      publicationDate <- c.get[Date]("publicationDate")
      lastUpdateTime <- c.get[Date]("lastUpdateTime")
      categories <- c.get[Set[String]]("categories")
      metrics <- c.get[Option[ArticleMetrics]]("metrics")
    } yield HabrArticle(id, link, title, description, author, categories, metrics, publicationDate, lastUpdateTime)
  }
}


