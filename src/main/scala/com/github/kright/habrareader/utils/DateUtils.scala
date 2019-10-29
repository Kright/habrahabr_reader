package com.github.kright.habrareader.utils

import java.text.SimpleDateFormat
import java.util.{Calendar, Date}

import io.circe.syntax._
import io.circe.{Decoder, Encoder}

import scala.concurrent.duration.FiniteDuration


object DateUtils {
  //todo may be rm this, store date as simple Long number in milliseconds

  def now: Date = Calendar.getInstance().getTime
  def convertToStr(date: Date, fmt: String = "yyyy-MM-dd HH:mm:ss.SSS Z"): String = new SimpleDateFormat(fmt).format(date)
  def currentDateStr(fmt: String = "yyyy-MM-dd HH:mm:ss.SSS Z"): String = convertToStr(now, fmt)
  def convertToDate(date: String, fmt: String = "yyyy-MM-dd HH:mm:ss.SSS Z"): Date = new SimpleDateFormat(fmt).parse(date)

  def getLast(left: Date, right: Date): Date =
    if (left.after(right))
      left
    else
      right

  implicit val dateEncoder: Encoder[Date] = (date: Date) => DateUtils.convertToStr(date).asJson

  implicit val dateDecoder: Decoder[Date] = Decoder[String].map(DateUtils.convertToDate(_))
}
