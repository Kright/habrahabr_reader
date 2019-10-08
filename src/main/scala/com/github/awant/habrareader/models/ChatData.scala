package com.github.awant.habrareader.models

import java.io.File
import java.util.Date

import org.slf4j.LoggerFactory

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object ChatData {

  case class Update(chat: Chat, post: Post, prevMessageId: Option[Int] = None) {
    def date: Date = post.updateDate
  }

  def empty(): ChatData =
    new ChatData(new mutable.HashMap(), new mutable.HashMap(), new ArrayBuffer())

  def load(file: File): ChatData = {
    // todo
    ???
  }
}

class ChatData(chatCollection: mutable.HashMap[Long, Chat],
               postCollection: mutable.HashMap[Long, Post],
               eventCollection: mutable.ArrayBuffer[Event]) {


  // todo rm events, add them into Chat info

  private val log = LoggerFactory.getLogger(classOf[ChatData])

  def getChat(id: Long): Chat = chatCollection.getOrElse(id, Chat.withDefaultSettings(id))

  def updateChat(chatId: Long)(mapFunc: Chat => Chat): Unit =
    chatCollection(chatId) = mapFunc(chatCollection.getOrElse(chatId, Chat.withDefaultSettings(chatId)))

  private def predicate(chat: Chat, post: Post): Boolean = {
    def getMeanOrZero(numbers: Seq[Double]): Double =
      if (numbers.isEmpty)
        0
      else
        numbers.sum / numbers.size

    val weight =
      (post.upVotes - post.downVotes) +
        chat.authorWeights.getOrElse(post.author, 0.0) +
        getMeanOrZero(post.categories.map(chat.tagWeights.getOrElse(_, 0.0)))

    weight >= chat.ratingThreshold
  }

  // todo rewrite logic
  private def getUpdates(chats: Seq[Chat], posts: Seq[Post], events: Seq[Event]): Seq[ChatData.Update] = {
    def getLastPost(left: Post, right: Post): Post =
      if (left.updateDate.after(right.updateDate)) left else right

    def getLastEvent(left: Event, right: Event): Event =
      if (left.updateDate.after(right.updateDate)) left else right

    val eventsByChat: Map[Long, Seq[Event]] = events.groupBy(_.chatId)

    val lastPosts: Iterable[Post] = posts.groupBy(_.id).map { case (_, posts) =>
      posts.reduce(getLastPost)
    }

    for {
      chat <- chats
      eventsByPostId = eventsByChat.getOrElse(chat.id, List()).groupBy(_.postId)
      post <- lastPosts
      relatedEvents = eventsByPostId.get(post.id)
      if relatedEvents.nonEmpty || predicate(chat, post)
    } yield ChatData.Update(chat, post, relatedEvents.map(_.reduce(getLastEvent).messageId))
  }

  def getUpdates(fromDate: Date): Seq[ChatData.Update] = {
    getUpdates(chatCollection.values.filter(_.subscription).toSeq,
      postCollection.values.filter(_.updateDate.after(fromDate)).toSeq, eventCollection)
  }

  def updatePosts(posts: Seq[Post]): Unit =
    posts.foreach(updatePost)

  def updatePost(post: Post): Unit =
    postCollection(post.id) = post

  def addEvent(event: Event): Unit =
    eventCollection += event
}
