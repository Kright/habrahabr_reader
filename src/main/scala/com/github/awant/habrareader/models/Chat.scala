package com.github.awant.habrareader.models

import java.util.Date

import com.github.awant.habrareader.utils.DateUtils
import com.github.awant.habrareader.utils.DateUtils._
import io.circe._
import io.circe.syntax._


case class Chat(id: Long,
                lastUpdateDate: Date,
                filterSettings: FilterSettings,
                sentArticles: Map[Long, SentArticle] = Map.empty) {

  private def prettyMap(map: Map[String, Double]): String =
    if (map.nonEmpty)
      map.toList.map { case (name, weight) => s"- $name: $weight" }.mkString("\n", "\n", "")
    else
      ""

  def getSettingsPrettify: String =
    s"""instant updates: ${filterSettings.updateAsSoonAsPossible}
       |authors weights: ${prettyMap(filterSettings.authorWeights)}
       |tags weights: ${prettyMap(filterSettings.tagWeights)}
       |rating threshold: ${filterSettings.ratingThreshold}
    """.stripMargin
}

object Chat {
  def withDefaultSettings(id: Long) =
    Chat(id, DateUtils.currentDate, filterSettings = FilterSettings(ratingThreshold = 20.0))

  implicit val encoder: Encoder[Chat] = (chat: Chat) =>
    Json.obj(
      "id" := chat.id,
      "lastUpdateDate" := chat.lastUpdateDate,
      "filterSettings" := chat.filterSettings,
      "sentPosts" := chat.sentArticles,
    )

  implicit val decoder: Decoder[Chat] = (c: HCursor) => {
    for {
      id <- c.get[Long]("id")
      lastUpdateDate <- c.get[Date]("lastUpdateDate")
      filterSettings <- c.get[FilterSettings]("filterSettings")
      sentPosts <- c.get[Map[Long, SentArticle]]("sentPosts")
    } yield Chat(id, lastUpdateDate, filterSettings, sentPosts)
  }
}
