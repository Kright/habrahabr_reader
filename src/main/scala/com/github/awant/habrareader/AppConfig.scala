package com.github.awant.habrareader

import com.github.awant.habrareader.utils.ConfigLoader
import com.typesafe.config.{Config, ConfigFactory}
import pureconfig.generic.auto._

object AppConfig {

  final case class AppConfig(tgbot: TgBotActorConfig,
                             shop: ShopActorConfig,
                             library: LibraryActorConfig)

  final case class ProxyConfig(ip: String, port: Int)
  final case class TgBotActorConfig(token: String, proxy: ProxyConfig)
  final case class ShopActorConfig(articlesUpdateTimeSeconds: Int)
  final case class LibraryActorConfig(chatsUpdateTimeSeconds: Int, savesDir: String)

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