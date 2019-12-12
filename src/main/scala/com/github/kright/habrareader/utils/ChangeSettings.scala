package com.github.kright.habrareader.utils

import com.github.kright.habrareader.models.{Chat, FilterSettings}

import scala.collection.mutable.ArrayBuffer
import scala.util.Try

sealed trait ChangeSettings

object ChangeSettings {

  case class AuthorRating(author: String, rating: Double) extends ChangeSettings

  case class TagRating(tag: String, rating: Double) extends ChangeSettings

  case class RatingThreshold(value: Double) extends ChangeSettings

  case class ChangeSubscription(newValue: Boolean) extends ChangeSettings

  case object Reset extends ChangeSettings

  private def tokenize(text: String): Array[String] = text.split("\\s+").filter(_.nonEmpty)

  private def updateSettings(updater: FilterSettings => FilterSettings): Chat => Chat =
    chat => chat.copy(filterSettings = updater(chat.filterSettings))

  def applyCmd(cmd: ChangeSettings): Chat => Chat = {
    cmd match {
      case Reset => c => Chat.withDefaultSettings(c.id)
      case ChangeSubscription(newValue) =>
        updateSettings(settings => settings.copy(updateAsSoonAsPossible = newValue))
      case AuthorRating(name, weight) =>
        updateSettings(s => s.copy(authorWeights = s.authorWeights.updated(name, weight)))
      case TagRating(name, weight) =>
        updateSettings(s => s.copy(tagWeights = s.tagWeights.updated(name, weight)))
      case RatingThreshold(value) =>
        updateSettings(_.copy(ratingThreshold = value))
    }
  }

  def concatCommands(cmds: Seq[ChangeSettings]): Chat => Chat = initialChatState =>
    cmds.map(applyCmd).foldLeft(initialChatState) { case (chat, modifier) => modifier(chat) }

  def parse(text: String): Seq[ChangeSettings] = {
    def toDouble: Option[String] => Option[Double] = _.flatMap(s => Try(s.toDouble).toOption)

    def toString: Option[String] => Option[String] = _.filter(!_.startsWith("/")).map(TextNormalization.normalize)

    val result = new ArrayBuffer[ChangeSettings]()
    val tokens = tokenize(text.toLowerCase)

    for ((token, pos) <- tokens.zipWithIndex) {

      token match {
        case "/author" =>
          for {
            name <- toString(tokens.lift(pos + 1))
            value <- toDouble(tokens.lift(pos + 2))
          } result += AuthorRating(name, value)

        case "/tag" =>
          for {
            name <- toString(tokens.lift(pos + 1))
            value <- toDouble(tokens.lift(pos + 2))
          } result += TagRating(name, value)

        case "/rating" =>
          for {
            value <- toDouble(tokens.lift(pos + 1))
          } result += RatingThreshold(value)

        case "/reset" =>
          result += Reset

        case "/subscribe" =>
          result += ChangeSubscription(true)

        case "/unsubscribe" =>
          result += ChangeSubscription(false)

        case _ => // skip
      }
    }

    result
  }
}
