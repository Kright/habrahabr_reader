package com.github.kright.habrareader.models

import com.github.kright.habrareader.utils.TextNormalization
import io.circe.syntax._
import io.circe.{Decoder, Encoder, HCursor, Json}

case class FilterSettings(authorWeights: Map[String, Double] = Map.empty,
                          tagWeights: Map[String, Double] = Map.empty,
                          ratingThreshold: Double = 0.0,
                          updateAsSoonAsPossible: Boolean = false) {

  val tagWeightsNormalized = tagWeights.map {
    case (tag, weight) => (TextNormalization.normalize(tag), weight)
  }

  def isInteresting(article: HabrArticle): Boolean = {
    def getMeanOrZero(numbers: Seq[Double]): Double =
      if (numbers.isEmpty)
        0
      else
        numbers.sum / numbers.size

    val weight =
      article.metrics.map(m => m.upVotes - m.downVotes).getOrElse(0) +
        authorWeights.getOrElse(article.author, 0.0) +
        getMeanOrZero(article.categoriesNormalized.toSeq.map(tagWeightsNormalized.getOrElse(_, 0.0)))

    weight >= ratingThreshold
  }
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
