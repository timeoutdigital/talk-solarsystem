/*
 * Copyright (C) 2013 Time Out Group Limited
 * All rights reserved.
 */

package com.timeout
package common

trait EnumContainer[T <: BaseEnum] {

  def values: Set[T]

  def fromString(s: String): Validation[String, T] = {
    val normalised = (s == null) ? (null, s.toLowerCase)
    values.find(_.id == normalised).toValidation {
      val expectedValues = values.map(_.id).mkString(", ")
      s"Unexpected value received '$s', expected one of ($expectedValues) (case-insensitive)."
    }
  }

}
