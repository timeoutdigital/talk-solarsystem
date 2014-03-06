package com.timeout
package common

import scala.language.implicitConversions
import PartialFunction._

import net.liftweb.json.JsonAST._
import net.liftweb.json.JsonDSL._

case class Page(number: Int = 1, size: Int = 10) {
  require(number > 0, "Page number must be higher than 0.")
  require(size > 0, "Page size must be higher than 0.")

  val from = (number - 1) * size

  def apply[T](xs: Iterable[T]): Iterable[T] = xs.slice(from, from + size)

  def previous: Option[Page] = (number > 1).toOption(Page(number - 1, size))
  def next = Page(number + 1, size)
  def hasNext(total: Long) = totalPages(total) > number

  def totalPages(totalItems: Long): Long = (totalItems - 1) / size + 1

  def toParams: Seq[(String, String)] = (Page.number, number.toString) :: (Page.size, size.toString) :: Nil
}

object Page {
  val number = "page_number"
  val size = "page_size"

  val params = Seq(number, size)

  def fromParams(number: Option[Int], size: Option[Int]) = (number, size) match {
    case (Some(n), Some(s)) => Page(n, s)
    case (Some(n), _) => Page(number = n)
    case (_, Some(s)) => Page(size = s)
    case _ => Page()
  }

}

