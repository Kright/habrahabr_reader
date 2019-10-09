package com.github.awant.habrareader

import java.io.File
import java.util.{Calendar, Date, TimeZone}

import com.github.awant.habrareader.Implicits._
import com.github.awant.habrareader.loaders.HabrArticlesDownloader
import com.github.awant.habrareader.utils.DateUtils
import org.scalatest.{FunSuite, Ignore}


class HabrParserTest extends FunSuite {

  test("testRssParse") {
    val file = new File(getClass.getClassLoader.getResource("exampleOfHabrRss.xml").getFile)

    assert(file.exists())
    val result = HabrArticlesDownloader.parseRss(file.text)

    assert(result.nonEmpty)

    assert(result.nonEmpty)

    val first = result.head

    assert(first.id == 461617)
    assert(first.link == "https://habr.com/ru/post/461617/")
    assert(first.description.startsWith("День добрый, Хабр! <br/>"))
    assert(first.author == "TimurBidzhiev")
    assert(first.publicationDate.before(new Date()))
    assert(first.categories.contains("Управление проектами"))
    assert(first.categories.contains("обучение"))
    assert(first.categories.contains("стартапы"))
    assert(first.categories.size == 7)
  }

  test("parseDate") {
    val string = "Sun, 28 Jul 2019 13:23:13 GMT"
    val date = HabrArticlesDownloader.parseDate(string)
    val c = Calendar.getInstance(TimeZone.getTimeZone("GMT"))
    c.setTime(date)

    assert(c.get(Calendar.DAY_OF_MONTH) == 28)
    assert(c.get(Calendar.MONTH) == Calendar.JULY)
    assert(c.get(Calendar.YEAR) == 2019)
    assert(c.get(Calendar.HOUR_OF_DAY) == 13)
    assert(c.get(Calendar.MINUTE) == 23)
    assert(c.get(Calendar.SECOND) == 13)
  }

  test("testHtmlParse") {
    val file = new File(getClass.getClassLoader.getResource("habr_en.html").getFile)
    val article = HabrArticlesDownloader.parseHtml(file.text, DateUtils.currentDate)

    assert(article.id == 462783)
    assert(article.link == "https://habr.com/ru/company/parallels/blog/462783/")
    assert(article.author == "SmirkinDA")
    assert(article.title == "Матрица: 20 лет спустя")
    assert(article.categories == Set("parallels", "martix", "history", "movie"))
    assert(article.description == "В этом году фанаты научной фантастики отмечают 20-летие с даты премьеры трилогии «Матрица». Кстати, вы знали, что в США фильм увидели в марте, а до нас он доехал лишь в октябре 1999 года? На...")
    assert(article.metrics.exists(_.upVotes == 43))
    assert(article.metrics.exists(_.downVotes == 36))
    assert(article.metrics.exists(_.viewsCount == 27300))
    assert(article.metrics.exists(_.commentsCount == 115))
    assert(article.metrics.exists(_.bookmarksCount == 37))
  }

}

@Ignore
class ExternalSuite extends FunSuite {
  test("loding rss articles") {
    val articles = HabrArticlesDownloader.downloadRSSArticles
    articles.foreach(println(_))
  }

  test("html parsing") {
    // Malformed input for articles:
    // https://habr.com/ru/post/465703/
    val article = HabrArticlesDownloader.downloadArticle("https://habr.com/ru/post/465703/", DateUtils.yesterday)
    println(article)
  }

  test("download articles") {
    val article = HabrArticlesDownloader.getArticles()
    article.foreach(println)
  }
}
