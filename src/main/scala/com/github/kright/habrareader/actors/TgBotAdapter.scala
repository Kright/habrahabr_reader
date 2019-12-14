package com.github.kright.habrareader.actors

import java.net.{InetSocketAddress, Proxy}

import akka.actor.ActorRef
import com.bot4s.telegram.api.RequestHandler
import com.bot4s.telegram.api.declarative.Messages
import com.bot4s.telegram.future.{Polling, TelegramBot}
import com.github.kright.habrareader.AppConfig.TgBotActorConfig
import com.github.kright.habrareader.actors.TgBotActor.ReceiveTgMessage
import com.github.kright.habrareader.utils.CustomScalajHttpClient

import scala.concurrent.{ExecutionContext, Future}

/**
  * resend tg messages to actor
  */

class TgBotAdapter(override val client: RequestHandler[Future], observer: ActorRef) extends TelegramBot with Polling with Messages[Future] {
  onExtMessage { case (message, botUser) =>
    Future {
      observer ! ReceiveTgMessage(message)
    }
  }
}

object TgBotAdapter {
  def apply(config: TgBotActorConfig, observer: ActorRef)(implicit ec: ExecutionContext): TgBotAdapter =
    new TgBotAdapter(new CustomScalajHttpClient(config.token, makeProxy(config)), observer)

  private def makeProxy(config: TgBotActorConfig): Proxy =
    if (config.proxy.ip.isEmpty)
      Proxy.NO_PROXY
    else
      new Proxy(Proxy.Type.SOCKS, InetSocketAddress.createUnresolved(config.proxy.ip, config.proxy.port))
}
