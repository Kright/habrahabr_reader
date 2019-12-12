package com.github.kright.habrareader.models

import io.circe.syntax._
import io.circe.{Decoder, Encoder, HCursor, Json}

case class FilterSettings(authorWeights: Weights = Weights(),
                          tagWeights: Weights = Weights(),
                          companyWeights: Weights = Weights(),
                          ratingThreshold: Double = 0.0,
                          updateAsSoonAsPossible: Boolean = false) {

  def isInteresting(article: HabrArticle): Boolean = {
    def getMeanOrZero(numbers: Seq[Double]): Double =
      if (numbers.isEmpty)
        0
      else
        numbers.sum / numbers.size

    val weight =
      article.metrics.map(m => m.upVotes - m.downVotes).getOrElse(0) +
        authorWeights(article.authorNormalized) +
        getMeanOrZero(article.categoriesNormalized.toSeq.map(tagWeights(_))) +
        article.company.map(companyWeights(_)).getOrElse(0.0)

    weight >= ratingThreshold
  }
}

object FilterSettings {
  implicit val encoder: Encoder[FilterSettings] = (settings: FilterSettings) =>
    Json.obj(
      "authorWeights" := settings.authorWeights,
      "tagWeights" := settings.tagWeights,
      "companyWeights" := settings.companyWeights,
      "ratingThreshold" := settings.ratingThreshold,
      "updateAsSoonAsPossible" := settings.updateAsSoonAsPossible,
    )

  implicit val decoder: Decoder[FilterSettings] = (c: HCursor) => {
    for {
      authorWeights <- c.get[Weights]("authorWeights")
      tagWeights <- c.get[Weights]("tagWeights")
      companyWeights <- c.get[Option[Weights]]("companyWeights")
      ratingThreshold <- c.get[Double]("ratingThreshold")
      updateAsSoonAsPossible <- c.get[Boolean]("updateAsSoonAsPossible")
    } yield FilterSettings(authorWeights, tagWeights, companyWeights.getOrElse(Weights()), ratingThreshold, updateAsSoonAsPossible)
  }
}
