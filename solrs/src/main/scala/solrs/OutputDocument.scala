/*
 * Copyright (C) 2013 Time Out Group Limited
 * All rights reserved.
 */

package com.timeout
package solrs

import scala.collection.JavaConverters._
import scala.reflect.classTag
import scala.reflect.ClassTag
import reflect.safeCast

import net.liftweb.json.JsonAST.{JObject, JField}

import org.apache.solr.common.{SolrDocument => SolrOutputDocument}

/** This is useful in cases one simply wants to pass through the data that is coming from Solr. In
  * other cases, prefer SolrDocument. */
final class OutputDocument(val underlying: SolrOutputDocument) extends Document { doc =>
  def size: Int = underlying.size

  def apply[T](field: Field[T])(implicit reader: Field.Reader[T]): T = get(field).get
  def get[T](field: Field[T])(implicit reader: Field.Reader[T]): Option[T] = reader.get(this)(field.name)

  def first(fieldName: String): Option[Any] = Option(underlying.getFirstValue(fieldName))

  def all(fieldName: String): Iterable[Any] = underlying.get(fieldName) match {
    case null => Seq.empty
    case v: java.util.Collection[_] => v.asScala
    case v => Seq(v)
  }

  def get(fieldName: String): Option[Any] = Option(maybeWrap(underlying.get(fieldName)))

  def apply(fieldName: String): Any = maybeWrap(underlying.get(fieldName)) match {
    case null => fieldNameNotFound(fieldName)
    case v => v
  }

  /** Returns the raw data for the field, either an Iterable or a single value. */
  def asRawMap: scala.collection.Map[String, Any] = new BaseMap[Any] {

    def get(key: String): Option[Any] = doc.get(key)

    override def apply(key: String): Any = underlying.get(key) match {
      case null => default(key)
      case v => maybeWrap(v)
    }

    def iterator: Iterator[(String, Any)] = new Iterator[(String, Any)] {
      val it = underlying.getFieldNames.iterator
      def hasNext: Boolean = it.hasNext
      def next: (String, Any) = {
        val key = it.next
        (key, apply(key))
      }
    }
  }

  /** Returns the first values if there are multiple values for a given key. */
  def asSingleMap: scala.collection.Map[String, Any] = new BaseMap[Any] {

    def get(key: String): Option[Any] = first(key)

    override def apply(key: String): Any = underlying.getFirstValue(key) match {
      case null => default(key)
      case v => v
    }

    def iterator: Iterator[(String, Any)] = new Iterator[(String, Any)] {
      val it = underlying.getFieldNames.iterator
      def hasNext: Boolean = it.hasNext
      def next: (String, Any) = {
        val key = it.next
        (key, apply(key))
      }
    }
  }

  /** Returns empty Iterable if the key is not present. */
  def asMultiMap: scala.collection.Map[String, Iterable[Any]] = new BaseMap[Iterable[Any]] {
    override def apply(key: String): Iterable[Any] = all(key)
    override def default(key: String) = Seq.empty

    def get(key: String): Option[Iterable[Any]] = Option(apply(key))
    def iterator: Iterator[(String, Iterable[Any])] = new Iterator[(String, Iterable[Any])] {
      val it = underlying.getFieldNames.iterator
      def hasNext: Boolean = it.hasNext
      def next: (String, Iterable[Any]) = {
        val key = it.next
        (key, apply(key))
      }
    }
  }

  private abstract class BaseMap[V] extends scala.collection.Map[String, V] {
    def -(key: String): scala.collection.Map[String, V] = 
      scala.collection.mutable.Map.empty ++= this -= key
    def +[B1 >: V](kv: (String, B1)): scala.collection.Map[String, B1] = 
      scala.collection.mutable.Map.empty[String, B1] ++= this += kv
  }

  protected def fieldNameNotFound(fieldName: String): Nothing =
    throw new NoSuchElementException(s"Mandatory field '$fieldName' not found in Solr document: $underlying")

}
