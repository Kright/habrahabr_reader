package com.github.kright.habrareader.models

import com.github.kright.habrareader.utils.{DateUtils, Saver, SavesDir}

class StateSaverFS(saveDir: String, log: String => Unit) extends Saver[State] {
  private val dir = new SavesDir(saveDir)

  override def load(): State =
    dir.loadLast().map { file =>
      log(s"load previous state from ${file.getAbsolutePath}")
      State.load(file)
    }.getOrElse {
      log(s"previous save wasn't found, use empty")
      State.empty()
    }

  override def save(state: State): Unit = {
    val dest = dir.newSave(DateUtils.now)
    dest.getParentFile.mkdirs()
    State.save(state, dest)
    log(s"state saved to $dest")
  }
}
