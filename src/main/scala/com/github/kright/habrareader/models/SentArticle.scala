package com.github.kright.habrareader.models

import java.util.Date

import com.github.kright.habrareader.utils.DateUtils._
import io.circe._
import io.circe.syntax._

case class SentArticle(messageId: Int,
                       articleId: Int,
                       sentDate: Date)

object SentArticle {

  implicit val encoder: Encoder[SentArticle] = (sentArticle: SentArticle) =>
    Json.obj(
      "messageId" := sentArticle.messageId,
      "articleId" := sentArticle.articleId,
      "sentDate" := sentArticle.sentDate,
    )

  implicit val decoder: Decoder[SentArticle] = (c: HCursor) =>
    for {
      messageId <- c.get[Int]("messageId")
      articleId <- c.get[Int]("articleId")
      sentDate <- c.get[Date]("sentDate")
    } yield SentArticle(messageId, articleId, sentDate)
}
