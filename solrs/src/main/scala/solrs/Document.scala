/*
 * Copyright (C) 2013 Time Out Group Limited
 * All rights reserved.
 */

package com.timeout
package solrs

import scala.collection.JavaConverters._

trait Document {
  def all(fieldName: String): Iterable[Any]
  def get(fieldName: String): Option[Any]

  def apply[T](field: Field[T])(implicit reader: Field.Reader[T]): T
  def get[T](field: Field[T])(implicit reader: Field.Reader[T]): Option[T]

  protected def maybeWrap(value: Any): Any = value match {
    case v: java.util.Collection[_] => v.asScala
    case v => v
  }
}
