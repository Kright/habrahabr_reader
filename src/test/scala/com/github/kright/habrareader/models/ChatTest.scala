package com.github.kright.habrareader.models

import com.github.kright.habrareader.utils.ChangeSettings._
import org.scalatest.FunSuite

class ChatTest extends FunSuite {

  test("check parsing of generated command") {
    val chat = Chat(0, new FilterSettings(
      ratingThreshold = 12,
      tagWeights = Map("t1" -> 2.0, "t2" -> -1, "t3" -> 1.3),
      authorWeights = Map("aw" -> 1.0, "aw2" -> -2.0),
      updateAsSoonAsPossible = true
    ))

    val parsed = concatCommands(parse(chat.getSettingsAsCmd))

    assert(chat == parsed(Chat.withDefaultSettings(0)))
  }
}
