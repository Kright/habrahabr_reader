package com.github.kright.habrareader.models

import io.circe._
import io.circe.syntax._


case class Chat(id: Long,
                filterSettings: FilterSettings,
                sentArticles: Map[Int, SentArticle] = Map.empty) {

  def getSettingsAsCmd: String = {
    def toLines(w: Weights, cmd: String): Iterable[String] = w.normalized.map { case (name, weight) => s"$cmd $name $weight" }

    val cmds =
      toLines(filterSettings.authorWeights, "/author") ++
        toLines(filterSettings.tagWeights, "/tag") ++
        toLines(filterSettings.companyWeights, "/company")

    s"""${if (filterSettings.updateAsSoonAsPossible) "/subscribe" else "/unsubscribe"}
       |/rating ${filterSettings.ratingThreshold}
       |${cmds.mkString("\n")}""".stripMargin
  }
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
