package com.github.kright.habrareader.models

import io.circe._
import io.circe.syntax._


case class Chat(id: Long,
                filterSettings: FilterSettings,
                sentArticles: Map[Int, SentArticle] = Map.empty) {

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
    Chat(id, filterSettings = FilterSettings(ratingThreshold = 25.0))

  implicit val encoder: Encoder[Chat] = (chat: Chat) =>
    Json.obj(
      "id" := chat.id,
      "filterSettings" := chat.filterSettings,
      "sentPosts" := chat.sentArticles,
    )

  implicit val decoder: Decoder[Chat] = (c: HCursor) => {
    for {
      id <- c.get[Long]("id")
      filterSettings <- c.get[FilterSettings]("filterSettings")
      sentPosts <- c.get[Map[Int, SentArticle]]("sentPosts")
    } yield Chat(id, filterSettings, sentPosts)
  }
}
