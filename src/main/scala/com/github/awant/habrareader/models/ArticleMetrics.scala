package com.github.awant.habrareader.models

import io.circe.syntax._
import io.circe.{Decoder, Encoder, HCursor, Json}

case class ArticleMetrics(upVotes: Int,
                          downVotes: Int,
                          viewsCount: Int,
                          commentsCount: Int,
                          bookmarksCount: Int)

object ArticleMetrics {
  implicit val encoder: Encoder[ArticleMetrics] = (metrics: ArticleMetrics) => {
    Json.obj(
      "upvotes" := metrics.upVotes,
      "downvotes" := metrics.downVotes,
      "views" := metrics.viewsCount,
      "comments" := metrics.commentsCount,
      "bookmarks" := metrics.bookmarksCount,
    )
  }

  implicit val decoder: Decoder[ArticleMetrics] = (c: HCursor) => {
    for {
      upVotes <- c.get[Int]("upvotes")
      downVotes <- c.get[Int]("downvotes")
      viewsCount <- c.get[Int]("views")
      commentsCount <- c.get[Int]("comments")
      bookmarksCount <- c.get[Int]("bookmarks")
    } yield ArticleMetrics(upVotes, downVotes, viewsCount, commentsCount, bookmarksCount)
  }
}
