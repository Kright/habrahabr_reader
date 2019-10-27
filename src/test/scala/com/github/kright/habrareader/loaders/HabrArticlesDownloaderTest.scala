package com.github.kright.habrareader.loaders

import java.io.File
import java.nio.charset.{Charset, CodingErrorAction}
import java.util.{Calendar, Date, TimeZone}

import com.github.kright.habrareader.utils.DateUtils
import org.scalatest.FunSuite
import com.github.kright.habrareader.Implicits._
import com.github.kright.habrareader.models.ArticleMetrics

import scala.io.Codec

class HabrArticlesDownloaderTest extends FunSuite {
  test("testRssParse") {
    val file = new File(getClass.getClassLoader.getResource("exampleOfHabrRss.xml").getFile)

    assert(file.exists())
    val result = HabrArticlesDownloader.parseRss(file.text)

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

    assert(article.metrics.get == ArticleMetrics(
      upVotes = 43,
      downVotes = 36,
      viewsCount = 27300,
      commentsCount = 115,
      bookmarksCount = 37
    ))
  }

  test("testMalfornedHtmlParse") {
    val file = new File(getClass.getClassLoader.getResource("malformedUtf8Article").getFile)
    val text = file.text( Codec(Charset.forName("UTF-8")).onMalformedInput(CodingErrorAction.IGNORE))
    val article = HabrArticlesDownloader.parseHtml(text, DateUtils.currentDate)

    assert(article.id == 472278)
    assert(article.link == "https://habr.com/ru/company/flant/blog/472278/")
    assert(article.author == "may-cat")
    assert(article.title == "Мой любимый Git-коммит")
    assert(article.categories == Set("Git", "commit message"))
    assert(article.description == "Прим. перев.: Эта публикация британского программиста, ставшая настоящим хитом в англоязычном интернете, ссылается на Git-коммит 6-летней давности. Он был зафиксирован в одном из открытых...")

    assert(article.metrics.get == ArticleMetrics(
      upVotes = 121,
      downVotes = 6,
      viewsCount = 32800,
      commentsCount = 73,
      bookmarksCount = 139
    ))
  }
}
