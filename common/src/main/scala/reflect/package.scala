/*
 * Copyright (C) 2013 Time Out Group Limited
 * All rights reserved.
 */

package com.timeout

import scala.reflect._

package object reflect {

  def safeCast[T: ClassTag](x: Any): Option[T] = x match {
    case null => None
    case x => boxClassTag[T].unapply(x)
  }

  def boxClassTag[T: ClassTag]: ClassTag[T] =
    classTag[T].runtimeClass match {
      case java.lang.Boolean.TYPE => classTag[java.lang.Boolean].asInstanceOf[ClassTag[T]]
      case java.lang.Byte.TYPE => classTag[java.lang.Byte].asInstanceOf[ClassTag[T]]
      case java.lang.Character.TYPE => classTag[java.lang.Character].asInstanceOf[ClassTag[T]]
      case java.lang.Double.TYPE => classTag[java.lang.Double].asInstanceOf[ClassTag[T]]
      case java.lang.Float.TYPE => classTag[java.lang.Float].asInstanceOf[ClassTag[T]]
      case java.lang.Integer.TYPE => classTag[java.lang.Integer].asInstanceOf[ClassTag[T]]
      case java.lang.Long.TYPE => classTag[java.lang.Long].asInstanceOf[ClassTag[T]]
      case java.lang.Short.TYPE => classTag[java.lang.Short].asInstanceOf[ClassTag[T]]
      case _ => classTag[T]
    }

}



