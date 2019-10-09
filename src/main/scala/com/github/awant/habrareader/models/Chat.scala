package com.github.awant.habrareader.models

import java.util.Date

import com.github.awant.habrareader.utils.DateUtils
import com.github.awant.habrareader.utils.DateUtils._
import io.circe._
import io.circe.syntax._


case class Chat(id: Long,
                lastUpdateDate: Date,
                subscription: Boolean,
                authorWeights: Map[String, Double] = Map.empty,
                tagWeights: Map[String, Double] = Map.empty,
                ratingThreshold: Double = 0.0, // todo move to settings
                sentArticles: Map[Long, SentArticle] = Map.empty) {

  private def prettyMap(map: Map[String, Double]): String =
    if (map.nonEmpty)
      map.toList.map { case (name, weight) => s"- ${name}: ${weight}" }.mkString("\n", "\n", "")
    else
      ""

  def getSettingsPrettify: String =
    s"""subscription: $subscription
       |authors weights: ${prettyMap(authorWeights)}
       |tags weights: ${prettyMap(tagWeights)}
       |rating threshold: $ratingThreshold
    """.stripMargin
}

object Chat {
  def withDefaultSettings(id: Long) =
    Chat(id, DateUtils.currentDate, subscription = false, ratingThreshold = 10)

  implicit val encoder: Encoder[Chat] = (chat: Chat) =>
    Json.obj(
      "id" := chat.id,
      "lastUpdateDate" := chat.lastUpdateDate,
      "subscription" := chat.subscription,
      "authorWeights" := chat.authorWeights,
      "tagWeights" := chat.tagWeights,
      "ratingThreshold" := chat.ratingThreshold,
      "sentPosts" := chat.sentArticles,
    )

  implicit val decoder: Decoder[Chat] = (c: HCursor) => {
    for {
      id <- c.get[Long]("id")
      lastUpdateDate <- c.get[Date]("lastUpdateDate")
      subscription <- c.get[Boolean]("subscription")
      authorWeights <- c.get[Map[String, Double]]("authorWeights")
      tagWeights <- c.get[Map[String, Double]]("tagWeights")
      ratingThreshold <- c.get[Double]("ratingThreshold")
      sentPosts <- c.get[Map[Long, SentArticle]]("sentPosts")
    } yield Chat(id, lastUpdateDate, subscription, authorWeights, tagWeights, ratingThreshold, sentPosts)
  }
}
