/*
 * Copyright (C) 2012 Time Out Group Limited
 * All rights reserved.
 */

package com.timeout

import java.io.Closeable

package object common {
  implicit class RichBoolean(val boolean: Boolean) extends AnyVal {
    @inline def ?[T](t: => T, f: => T): T = if (boolean) t else f
    @inline def toOption[T](x: => T) = if (boolean) Some(x) else None
    @inline def toInt: Int = if (boolean) 1 else 0
  }

  implicit class RichOption[T](val o: Option[T]) extends AnyVal {
    def toValidation[E](e: => E) = o.fold[Validation[E, T]](Failure(e))(Success(_))
  }

  implicit class RichString(val x: String) extends AnyVal {
    def quote: String = "\"" + x + "\""
  }

  final def using[T <: Closeable, R](closeable: T)(f: T => R): R = {
    val result = {
      try f(closeable)
      catch {
        case outer: Throwable =>
          try closeable.close
          catch { case inner: Throwable => outer.addSuppressed(inner) }
          throw outer
      }
    }
    closeable.close
    result
  }
}



