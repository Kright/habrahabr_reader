package com.github.kright.habrareader.utils

import com.typesafe.config.{Config, ConfigFactory}
import pureconfig.ConfigReader

object ConfigLoader {
  def isResourceExists(name: String): Boolean = getClass.getClassLoader.getResource(name) != null

  def getConfig(defaultPath: String, localPath: String = ""): Config = {
    val conf = ConfigFactory.load(defaultPath)

    Option(localPath)
      .filter(isResourceExists)
      .map(name => ConfigFactory.load(name).withFallback(conf))
      .getOrElse(conf)
  }

  def getConfig[T](configPath: String)(defaultPath: String, localPath: String)
                  (implicit reader: ConfigReader[T]): T = {
    val config = getConfig(defaultPath, localPath)
    pureconfig.loadConfig[T](config.getConfig(configPath))
  }.right.get
}
