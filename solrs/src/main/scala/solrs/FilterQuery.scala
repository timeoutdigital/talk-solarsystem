
/*
 * Copyright (C) 2013 Time Out Group Limited
 * All rights reserved.
 */

package com.timeout
package solrs

case class FilterQuery(value: String, tag: Option[String] = None) {
  def toQueryParams: String = tag.fold(value)(t => s"{!tag=$t}$value")
}
