package com.github.awant.habrareader.utils

import java.io.File
import java.util.Date

import scala.util.Try

class SavesDir(baseDir: File) {
  baseDir.mkdirs()

  def this(baseDir: String) = this(new File(baseDir))

  def loadAll(): Array[File] = baseDir.listFiles().view.flatMap { file =>
    Try {
      (file, DateUtils.convertToDate(file.getName).getTime)
    }.toOption
  }.sortBy { case (file, time) => time }
    .map { case (file, time) => file }
    .toArray

  def loadLast(): Option[File] = loadAll().lastOption

  def newSave(date: Date): File = new File(baseDir, DateUtils.convertToStr(date))
}
