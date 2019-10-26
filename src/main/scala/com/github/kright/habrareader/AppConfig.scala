package com.github.kright.habrareader

import com.typesafe.config.{Config, ConfigFactory}
import pureconfig.generic.auto._

import scala.concurrent.duration._

object AppConfig {

  final case class AppConfig(tgbot: TgBotActorConfig,
                             articlesUpdater: ArticlesUpdaterConfig,
                             library: LibraryActorConfig)

  final case class ProxyConfig(ip: String, port: Int)

  final case class TgBotActorConfig(token: String, proxy: ProxyConfig, chatsUpdateIntervalSeconds: Int, admins: Set[Long], updateExistingMessages: Boolean) {
    def chatsUpdateInterval: FiniteDuration = chatsUpdateIntervalSeconds.seconds
  }

  final case class ArticlesUpdaterConfig(searchNewArticlesIntervalSeconds: Int,
                                         articlesUpdateIntervalSeconds: Int)

  final case class LibraryActorConfig(stateSaveIntervalSeconds: Int, savesDir: String) {
    def stateSaveInterval: FiniteDuration = stateSaveIntervalSeconds.seconds
  }

  def apply(): AppConfig = config

  def asUntyped: Config = untyped

  private lazy val untyped: Config = {
    val local = "local.conf"
    ConfigFactory.load(if (isResourceExists(local)) local else "application.conf")
  }

  private lazy val config: AppConfig = {
    val loaded = pureconfig.loadConfig[AppConfig](untyped)
    assert(loaded.isRight, s"can't load config: $loaded")
    loaded.right.get
  }

  private def isResourceExists(name: String): Boolean = getClass.getClassLoader.getResource(name) != null
}