package com.timeout
package solrs

import org.apache.solr.common.SolrInputDocument
import scala.collection.JavaConverters._

class InputDocument(private val underlying: SolrInputDocument) extends Document {
  def apply[T](field: Field[T])(implicit reader: Field.Reader[T]): T = ???
  def get[T](field: Field[T])(implicit reader: Field.Reader[T]): Option[T] = ???

  def asJava: SolrInputDocument = underlying
  override def toString: String = s"InputDocument(${underlying.toString})"

  // TODO Remove code duplication with OutputDocument
  def get(fieldName: String): Option[Any] = Option(maybeWrap(underlying.get(fieldName)))
  def all(fieldName: String): Iterable[Any] = underlying.get(fieldName) match {
    case null => Seq.empty
    case v: java.util.Collection[_] => v.asScala
    case v => Seq(v)
  }

}

object InputDocument {
  def apply(xs: Field.Operation*): InputDocument = fromSeq(xs)

  def fromSeq(xs: Seq[Field.Operation]): InputDocument = {
    val doc = new SolrInputDocument
    xs.foreach(_(doc))
    new InputDocument(doc)
  }
}
