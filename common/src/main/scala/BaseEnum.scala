/*
 * Copyright (C) 2013 Time Out Group Limited
 * All rights reserved.
 */

package com.timeout
package common

trait BaseEnum {
  def id: String = toString.toLowerCase
}
