/*
 * Copyright (C) 2012 Time Out Group Limited
 * All rights reserved.
 */

package com.timeout
package common

import scala.language.experimental.macros
import scala.reflect.macros.Context

/**
 * This macro return a set containing all objects implementing the given sealed type 'T'.
 *
 * ==Example==
 * {{{
 * sealed trait Foo
 * case object BarA extends Foo
 * case object BarB extends Foo
 *
 * scala> SealedObjects[Foo]
 * res0: scala.collection.immutable.Set[Foo] = Set(BarA, BarB)
 * }}}
 *
 * ==Remark==
 * The macro will find only case objects defined *before* the call (when used in the same compilation unit).
 *
 * If the enumerated types are enclosed in the same object as the macro call, be sure to give an explicit type:
 * {{{
 * object Bar {
 *   case object BarA extends Bar[Int]
 *   case object BarB extends Bar[String]
 * 
 *   lazy val values: Set[Bar[_]] = SealedObjects[Bar[_]]
 * }
 * }}}
 */
object SealedObjects {
  def apply[T]: Set[T] = macro applyImpl[T]

  def applyImpl[T: c.WeakTypeTag](c: Context): c.Expr[Set[T]] = {
    import c.universe._

    val symbol = weakTypeOf[T].typeSymbol
    val internal = symbol.asInstanceOf[scala.reflect.internal.Symbols#Symbol]    
    if (!internal.isSealed) c.abort(c.enclosingPosition, "This isn't a sealed type.")

    val descendants = internal.sealedDescendants.map(_.asInstanceOf[Symbol])

    val objs = (descendants - symbol).map(
      s => s.owner.typeSignature.member(s.name.toTermName)
    )

    if (objs.isEmpty) c.abort(c.enclosingPosition, "No objects found (please see scaladoc for more information).")

    c.Expr[Set[T]] {
      Apply(
        Select(
          Select(Select(Select(Ident(newTermName("scala")), newTermName("collection")), newTermName("immutable")), newTermName("Set")),
          newTermName("apply")
        ), 
        objs.map(Ident(_)).to[List]
      )
    }
  }
}
