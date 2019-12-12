package com.github.kright.habrareader.models

import com.github.kright.habrareader.utils.DateUtils

object HabrArticleDummy {
  def apply(id: Int, upVotes: Int = 25) = HabrArticle(
    id,
    s"link$id",
    s"title_$id",
    s"description_$id",
    s"author_$id",
    Set.empty,
    None,
    Some(ArticleMetrics(
      upVotes = upVotes,
      downVotes = 0,
      viewsCount = 0,
      commentsCount = 0,
      bookmarksCount = 0,
    )),
    DateUtils.now,
    DateUtils.now
  )
}
