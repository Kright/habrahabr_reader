package com.github.kright.habrareader.models

import io.circe.syntax._
import io.circe.{Decoder, Encoder, HCursor, Json}

case class FilterSettings(authorWeights: Map[String, Double] = Map.empty,
                          tagWeights: Map[String, Double] = Map.empty,
                          ratingThreshold: Double = 0.0,
                          updateAsSoonAsPossible: Boolean = false) {
}

object FilterSettings {
  implicit val encoder: Encoder[FilterSettings] = (settings: FilterSettings) =>
    Json.obj(
      "authorWeights" := settings.authorWeights,
      "tagWeights" := settings.tagWeights,
      "ratingThreshold" := settings.ratingThreshold,
      "updateAsSoonAsPossible" := settings.updateAsSoonAsPossible,
    )

  implicit val decoder: Decoder[FilterSettings] = (c: HCursor) => {
    for {
      authorWeights <- c.get[Map[String, Double]]("authorWeights")
      tagWeights <- c.get[Map[String, Double]]("tagWeights")
      ratingThreshold <- c.get[Double]("ratingThreshold")
      updateAsSoonAsPossible <- c.get[Boolean]("updateAsSoonAsPossible")
    } yield FilterSettings(authorWeights, tagWeights, ratingThreshold, updateAsSoonAsPossible)
  }
}
