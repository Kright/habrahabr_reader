package com.github.kright.habrareader.loaders

import java.nio.charset.{Charset, CodingErrorAction}
import java.text.SimpleDateFormat
import java.util.{Date, Locale, TimeZone}

import com.github.kright.habrareader.Implicits._
import com.github.kright.habrareader.models.{ArticleMetrics, HabrArticle}
import com.github.kright.habrareader.utils.DateUtils
import net.ruippeixotog.scalascraper.browser.JsoupBrowser
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.scraper.ContentExtractors.{element, elementList}

import scala.io.{Codec, Source}
import scala.util.{Success, Try}
import scala.xml.XML


object HabrArticlesDownloader {
  private val rssURI = "https://habr.com/ru/rss/all/all/"
  private val codec = Codec(Charset.forName("UTF-8")).onMalformedInput(CodingErrorAction.IGNORE)

  private def getTextFromUrl(url: String): String = Source.fromURL(url)(codec).use(_.getLines().mkString("\n"))

  /** may block thread or throw exceptions */
  def downloadRSSArticles: Seq[HabrArticle] = parseRss(getTextFromUrl(rssURI))

  /** may block thread or throw exceptions */
  def downloadArticle(url: String, pubDate: Date): HabrArticle = parseHtml(getTextFromUrl(url), pubDate)

  def parseRss(text: String): Seq[HabrArticle] = {
    val root = XML.loadString(text)
    val items = root \ "channel" \ "item"

    items.toList.map { item =>
      val link = (item \ "guid").text

      HabrArticle(
        id = link.split("/")(5).toInt,
        link = link,
        title = (item \ "title").text,
        description = (item \ "description").text,
        author = (item \ "creator").text,
        categories = (item \ "category").map(_.text).toSet,
        metrics = None,
        publicationDate = parseDate((item \ "pubDate").text),
        lastUpdateTime = DateUtils.currentDate,
      )
    }
  }

  // TODO: parse pubDate from html
  def parseHtml(htmlText: String, pubDate: Date): HabrArticle = {
    val browser = JsoupBrowser()
    val doc = browser.parseString(htmlText)

    val metaAttributes = (doc.head >> elementList("meta")).map(_.attrs)

    val description: String = metaAttributes.find { attrs =>
      attrs.get("name").contains("description") && attrs.contains("content")
    }.map(_ ("content")).getOrElse("").trim

    val categories: Set[String] = metaAttributes.find { attrs =>
      attrs.get("name").contains("keywords") && attrs.contains("content")
    }.map(_ ("content")).getOrElse("").split(", ").toSet

    val link: String = metaAttributes.find { attrs =>
      attrs.get("property").contains("og:url")
    }.map(_ ("content")).getOrElse("")

    val id: Int = link.split("/").filter(_.nonEmpty).last.toInt
    val author: String = doc >> text(".post__meta .user-info__nickname")

    val views: Int = {
      val s: String = doc >> text(".post-stats__views")
      if (s.endsWith("k")) {
        (s.replace(',', '.').substring(0, s.length - 1).toDouble * 1000).toInt
      } else {
        s.toInt
      }
    }

    val commentsCount = Try {
      (doc >> text(".post-stats__comments-count")).toInt
      // isn't exist if no comments
    }.getOrElse(0)

    val addedToBookmarks = (doc >> text(".bookmark__counter")).toInt

    val string = (doc >> element(".voting-wjt__counter")).attr("title")
    val arr = string.split(Array('↑', '↓')).map(s => s.filter(_.isDigit).toInt)
    val upvotes = arr(1)
    val downvotes = arr(2)
    val title = doc >> text(".post__title-text")

    HabrArticle(
      id = id,
      link = link,
      title = title,
      description = description,
      author = author,
      categories = categories,
      metrics = Some(ArticleMetrics(
        upVotes = upvotes,
        downVotes = downvotes,
        viewsCount = views,
        commentsCount = commentsCount,
        bookmarksCount = addedToBookmarks
      )),
      publicationDate = pubDate,
      lastUpdateTime = DateUtils.currentDate
    )
  }

  def parseDate(s: String): Date = dateFormat.parse(s)

  private val dateFormat = new SimpleDateFormat("EEE, dd MMM yyy HH:mm:ss 'GMT'", Locale.ENGLISH) {
    setTimeZone(TimeZone.getTimeZone("GMT"))
  }

  def getArticles(): Seq[HabrArticle] =
    downloadRSSArticles.map { imprint =>
      Try{downloadArticle(imprint.link, imprint.publicationDate)} }.collect { case Success(s) => s }

  def update(link: String, pubDate: Date): HabrArticle = downloadArticle(link, pubDate)
}
