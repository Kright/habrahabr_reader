package com.github.kright.habrareader.utils

trait Saver[T] {
  def load(): T

  def save(t: T): Unit
}
