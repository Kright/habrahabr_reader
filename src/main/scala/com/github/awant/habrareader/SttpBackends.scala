package com.github.awant.habrareader

import com.softwaremill.sttp.okhttp._

object SttpBackends {
  val default = OkHttpFutureBackend()
}
