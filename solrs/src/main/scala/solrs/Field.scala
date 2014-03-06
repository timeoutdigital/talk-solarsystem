/*
 * Copyright (C) 2013 Time Out Group Limited
 * All rights reserved.
 */

package com.timeout
package solrs

import scala.collection.JavaConverters._
import java.lang.{Iterable => JIterable}

import org.joda.time.DateTime
import org.apache.solr.common.SolrInputDocument
import org.apache.solr.common.{SolrDocument => SolrOutputDocument}

sealed trait Field[T] { def name: String }

object Field {
  class Single[T](val name: String, processor: T => T = identity[T] _) extends Field[T] {
    def :=(value: T)(implicit w: Writer[T]): Set[T] = Set(name, processor(value))
    final def ?=(value: Option[T])(implicit w: Writer[T]): Operation = value.fold[Operation](NoOp)(:= _)
    final def as[B](processor: B => B = identity[B] _): Single[B] = new Single(name, processor)
  }

  class Multi[T](name: String) extends Single[Seq[T]](name) {
    final def +=(value: T)(implicit w: Writer[T]): Operation = ++=(Seq(value))
    def ++=(values: Seq[T])(implicit w: Writer[T]): Operation = Add(name, values)
  }

  sealed trait Operation { def apply(document: SolrInputDocument): Unit }

  case object NoOp extends Operation { def apply(document: SolrInputDocument): Unit = {} }

  case class Add[T](name: String, values: Seq[T])(implicit writer: Writer[T])  extends Operation {
    def apply(document: SolrInputDocument): Unit = writer.add(document)(name, values)
  }

  case class Set[T](name: String, value: T)(implicit writer: Writer[T]) extends Operation {
    def apply(document: SolrInputDocument): Unit = writer.set(document)(name, value)
  }

  trait Reader[T] { 
    protected def fromSolr(x: Any): T = x.asInstanceOf[T]
    def get(document: Document)(name: String): Option[T]
  }

  object Reader {
    private def apply[T] = new Reader[T] {
      def get(document: Document)(name: String): Option[T] = document.get(name).map(fromSolr)
    }

    implicit val boolean      = Reader[Boolean]
    implicit val int          = Reader[Int]
    implicit val long         = Reader[Long]
    implicit val string       = Reader[String]
    implicit val double       = Reader[Double]
    implicit def seq[T](implicit reader: Reader[T]) = new Reader[Seq[T]] {
      def get(document: Document)(name: String): Option[Seq[T]] = 
        document.all(name).map(reader.fromSolr).toSeq match {
          case Nil => None
          case xs => Some(xs)
        }
    }
  }

  trait Writer[T] { self =>
    def set(document: SolrInputDocument)(name: String, value: T): Unit = 
      document.setField(name, toSolr(value))

    def add(document: SolrInputDocument)(name: String, values: Seq[T]): Unit = {
      val xs: Seq[T] = Option(document.getFieldValues(name)).map(_.asScala.toSeq.asInstanceOf[Seq[T]]).getOrElse(Nil)
      document.setField(name, (xs ++ values).distinct.toSeq.asJava)
    }

    def comap[U](f: U => T): Writer[U] = new Writer[U] {
      override def set(document: SolrInputDocument)(name: String, value: U): Unit = self.set(document)(name, f(value))
      override def add(document: SolrInputDocument)(name: String, values: Seq[U]): Unit = self.add(document)(name, values.map(f))
      override def toSolr(value: U): Any = self.toSolr(f(value))
    }

    def toSolr(value: T): Any = value
  }

  object Writer {
    private def apply[T] = new Writer[T] {}

    implicit val boolean      = Writer[Boolean]
    implicit val int          = Writer[Int]
    implicit val long         = Writer[Long]
    implicit val string       = Writer[String]
    implicit val double       = Writer[Double]
    implicit def jiterable[T] = Writer[JIterable[T]]
    implicit def iterable[T]  = jiterable[T].comap[Iterable[T]](_.asJava)
    implicit def seq[T]       = iterable[T].comap[Seq[T]](identity)
  }
}
