package com.github.kright.habrareader.actors

import akka.actor.ActorSystem
import akka.testkit.{ImplicitSender, TestKit}
import com.github.kright.habrareader.AppConfig.LibraryActorConfig
import com.github.kright.habrareader.actors.LibraryActor._
import com.github.kright.habrareader.actors.TgBotActor.SendMessageToTg
import com.github.kright.habrareader.models._
import com.github.kright.habrareader.utils.Saver
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}

class LibraryActorTest extends TestKit(ActorSystem("MySpec"))
  with ImplicitSender
  with WordSpecLike
  with Matchers
  with BeforeAndAfterAll {

  private val config = LibraryActorConfig(60, "dummy", 3)

  override def afterAll: Unit = {
    TestKit.shutdownActorSystem(system)
  }

  "Library actor" should {
    val saver = new Saver[State]() {

      override def load(): State = State.empty()

      override def save(t: State): Unit = {}
    }

    val libraryActor = system.actorOf(LibraryActor.props(config, saver))
    val chatId: Long = 12

    "should update settings" in {

      libraryActor ! UpdateChat(chatId, c => c.copy(
        filterSettings = FilterSettings(
          authorWeights = Weights("sly_g" -> -30.0),
          tagWeights = Weights("scala" -> 20.0),
          ratingThreshold = 25,
          updateAsSoonAsPossible = true
        )
      ), needConfirmation = true)

      expectMsg(Ok)
    }

    "should get settings" in {
      libraryActor ! GetSettings(chatId)

      expectMsg(SendMessageToTg(chatId,
        """/subscribe
          |/rating 25.0
          |/author sly_g -30.0
          |/tag scala 20.0""".stripMargin))
    }

    "should reply if no articles" in {
      libraryActor ! RequestUpdates(chatId)
      expectMsg(SendMessageToTg(chatId, "no new articles :("))
    }

    val firstArticle = HabrArticleDummy(id = 1, upVotes = 30)
    "should send article with huge rating" in {

      libraryActor ! UpdateArticle(firstArticle)
      libraryActor ! RequestUpdates(chatId)

      expectMsg(TgBotActor.UpdateArticle(chatId, firstArticle, None))
    }

    "should not send it twice" in {
      libraryActor ! PostWasSentToTg(chatId, SentArticle(1, firstArticle.id, firstArticle.lastUpdateTime))
      libraryActor ! RequestUpdates(chatId)
      expectMsg(SendMessageToTg(chatId, "no new articles :("))
    }

    val badArticle = HabrArticleDummy(id = 2, upVotes = 40).copy(author = "SLY_G")

    "should filter out bad author" in {
      libraryActor ! UpdateArticle(badArticle)
      libraryActor ! RequestUpdates(chatId)
      expectMsg(SendMessageToTg(chatId, "no new articles :("))
    }

    val interestingArticle = HabrArticleDummy(id = 3, upVotes = 10).copy(categories = Set("scala"))

    "should show interesting article" in {
      libraryActor ! UpdateArticle(interestingArticle)
      libraryActor ! RequestUpdates(chatId)
      expectMsg(TgBotActor.UpdateArticle(chatId, interestingArticle, None))
    }

    "shouldn't show it twice" in {
      libraryActor ! PostWasSentToTg(chatId, SentArticle(1, interestingArticle.id, interestingArticle.lastUpdateTime))
      libraryActor ! RequestUpdates(chatId)
      expectMsg(SendMessageToTg(chatId, "no new articles :("))
    }
  }
}
