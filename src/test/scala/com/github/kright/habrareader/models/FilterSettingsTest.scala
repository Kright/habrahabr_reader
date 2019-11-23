package com.github.kright.habrareader.models

import org.scalatest.FunSuite

class FilterSettingsTest extends FunSuite {

  test("testCopy2") {
    val filter = new FilterSettings(
      ratingThreshold = 25,
      authorWeights = Map("SLY_G" -> -30.0),
      tagWeights = Map("Scala" -> 30.0),
    )

    assert(filter.authorWeightsNormalized == Map("sly_g" -> -30.0))
    assert(filter.tagWeightsNormalized == Map("scala" -> 30.0))

    val filter2 = new FilterSettings(ratingThreshold = 25)
      .copy(authorWeights = Map("SLY_G" -> -30.0))
      .copy(tagWeights = Map("Scala" -> 30.0))

    assert(filter2.authorWeightsNormalized == Map("sly_g" -> -30.0))
    assert(filter2.tagWeightsNormalized == Map("scala" -> 30.0))

    assert(filter == filter2)
  }

  test("testIsInteresting") {
    val filter = new FilterSettings(
      ratingThreshold = 25,
      authorWeights = Map("sly_g" -> -30.0),
      tagWeights = Map("scala" -> 30.0),
    )

    assert(filter.isInteresting(HabrArticleDummy(id = 1, upVotes = 30)))
    assert(!filter.isInteresting(HabrArticleDummy(id = 2, upVotes = 40).copy(author = "SLY_G")))
    assert(filter.isInteresting(HabrArticleDummy(id = 3, upVotes = 10).copy(categories = Set("scala"))))
  }

}
