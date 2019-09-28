package com.github.awant.habrareader.mongodb

import ch.qos.logback.classic.{Level, LoggerContext}
import com.github.awant.habrareader.AppConfig.MongoConfig
import com.github.awant.habrareader.models.{Chat, Event, Post}
import org.bson.codecs.configuration.CodecRegistries._
import org.bson.codecs.configuration.CodecRegistry
import org.mongodb.scala._
import org.mongodb.scala.bson.codecs.DEFAULT_CODEC_REGISTRY
import org.mongodb.scala.bson.codecs.Macros._
import org.slf4j.LoggerFactory

class Mongo(config: MongoConfig) {
  if (!config.writeLogs) {
    Mongo.disableLogs()
  }

  val mongoClient: MongoClient = MongoClient(config.uri)
  val codecRegistry: CodecRegistry = fromRegistries(fromProviders(classOf[Chat], classOf[Post], classOf[Event]), DEFAULT_CODEC_REGISTRY)
  val database: MongoDatabase = mongoClient.getDatabase(config.database).withCodecRegistry(codecRegistry)

  val chatCollection: MongoCollection[Chat] = database.getCollection[Chat]("chats")
  val postCollection: MongoCollection[Post] = database.getCollection[Post]("posts")
  val eventCollection: MongoCollection[Event] = database.getCollection[Event]("events")
}

object Mongo {
  def disableLogs(): Unit = {
    val loggerContext = LoggerFactory.getILoggerFactory.asInstanceOf[LoggerContext]
    val rootLogger = loggerContext.getLogger("org.mongodb.driver")
    rootLogger.setLevel(Level.OFF)
  }
}
