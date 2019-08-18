package com.github.awant.habrareader.habr

import java.util.Date

import cats.Semigroup
import cats.implicits._
import cats.instances.option._

case class HabrArticle(id: HabrArticle.Id,
                       link: String,
                       title: String,
                       description: String,
                       author: String,
                       date: Option[Date],
                       categories: Set[String],
                       fullText: Option[String],
                       rating: Option[ArticleStatistics]) {

  override def hashCode(): Int = id
}

object HabrArticle {
  type Id = Int

  implicit val semigroup = new Semigroup[HabrArticle] {
    override def combine(older: HabrArticle, newer: HabrArticle): HabrArticle = {
      assert(older.id == newer.id)

      newer.copy(
        date = newer.date.orElse(older.date),
        fullText = newer.fullText.orElse(older.fullText),
        rating = older.rating |+| newer.rating
      )
    }
  }
}