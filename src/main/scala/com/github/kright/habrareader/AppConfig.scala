package com.github.kright.habrareader

import com.github.kright.habrareader.utils.ConfigLoader
import com.typesafe.config.{Config, ConfigFactory}
import pureconfig.generic.auto._

import scala.concurrent.duration._

object AppConfig {

  final case class AppConfig(tgbot: TgBotActorConfig,
                             articlesUpdater: ArticlesUpdaterConfig,
                             library: LibraryActorConfig)

  final case class ProxyConfig(ip: String, port: Int)

  final case class TgBotActorConfig(token: String, proxy: ProxyConfig, chatsUpdateIntervalSeconds: Int, admins: Set[Long]) {
    def chatsUpdateInterval: FiniteDuration = chatsUpdateIntervalSeconds.seconds
  }

  final case class ArticlesUpdaterConfig(articlesUpdateIntervalSeconds: Int) {
    def articlesUpdateInterval: FiniteDuration = articlesUpdateIntervalSeconds.seconds
  }

  final case class LibraryActorConfig(stateSaveIntervalSeconds: Int, savesDir: String) {
    def stateSaveInterval: FiniteDuration = stateSaveIntervalSeconds.seconds
  }

  def apply(): AppConfig = config

  def asUntyped: Config = untyped

  private lazy val untyped: Config = {
    val configNames: Seq[String] = {
      val isServer = sys.env.get("HABRA_READER_SERVER").isDefined

      if (isServer)
        Seq("prod.conf", "application.conf")
      else
        Seq("local.conf", "application.conf")
    }.filter(ConfigLoader.isResourceExists)

    configNames.map(ConfigFactory.load).reduce(_.withFallback(_))
  }

  private lazy val config: AppConfig = {
    val loaded = pureconfig.loadConfig[AppConfig](untyped)
    println(s"loaded config = $loaded")
    loaded.right.get
  }
}