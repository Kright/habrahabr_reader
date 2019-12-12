package com.github.kright.habrareader.models

import com.github.kright.habrareader.utils.TextNormalization
import io.circe.syntax._
import io.circe.{Decoder, Encoder, HCursor}

class Weights(raw: Map[String, Double] = Map.empty) {

  val normalized: Map[String, Double] = raw.map {
    case (name, weight) => (TextNormalization.normalize(name), weight)
  }

  def updated(key: String, value: Double): Weights =
    if (value == 0.0)
      Weights(normalized - key)
    else
      Weights(normalized.updated(key, value))

  def apply(key: String): Double =
    normalized.getOrElse(TextNormalization.normalize(key), 0.0)

  override def equals(obj: Any): Boolean = obj match {
    case w: Weights => normalized == w.normalized
  }

  override def hashCode(): Int = normalized.hashCode()
}

object Weights {
  implicit val encoder: Encoder[Weights] =
    (w: Weights) => w.normalized.asJson

  implicit val decoder: Decoder[Weights] =
    (c: HCursor) => c.as[Map[String, Double]].map(new Weights(_))

  def apply(raw: Map[String, Double] = Map.empty): Weights = new Weights(raw)

  def apply(pairs: (String, Double)*): Weights = new Weights(Map(pairs: _*))
}
