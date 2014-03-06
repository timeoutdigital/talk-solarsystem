/*
 * Copyright (C) 2013 Time Out Group Limited
 * All rights reserved.
 */

package com.timeout
package solrs

import org.joda.time.{DateTime, DateTimeZone}
import org.joda.time.format.ISODateTimeFormat
import org.apache.solr.common.params.FacetParams._

/*
 * Even though this is like a BaseParam in many ways, it's not intended to be used outside of FacetParams,
 * so it doesn't inherit from BaseParam.
 */
sealed trait RangeFacet[B, G] {

  def fieldName: String
  def outputKey: Option[String]

  def outputName: String = outputKey.getOrElse(fieldName)

  protected def start: B
  protected def end: B
  protected def gap: G
  protected def other: String

  protected def boundAsText(b: B): String
  protected def gapAsText(g: G): String

  def withOutputKey(s: Option[String]): RangeFacet[B, G]

  def toSolrQuery: Seq[(String, Seq[String])] = {

    def key(rangeParamName: String): String =
      s"f.${fieldName}.${rangeParamName}"

    Seq(
      FACET_RANGE -> outputKey.fold(fieldName)(key => s"{!key=$key}$fieldName"),
      key(FACET_RANGE_START) -> boundAsText(start),
      key(FACET_RANGE_END) -> boundAsText(end),
      key(FACET_RANGE_GAP) -> gapAsText(gap),
      key(FACET_RANGE_OTHER) -> other
    ).map { case (n, v) => n -> Seq(v) }

  }
}

case class DateRangeFacet(fieldName: String, start: DateTime, end: DateTime, gap: String, other: String, outputKey: Option[String] = None) extends RangeFacet[DateTime, String] {
  protected def boundAsText(d: DateTime) = ISODateTimeFormat.dateTimeNoMillis.withZone(DateTimeZone.UTC).print(d)
  protected def gapAsText(g: String) = g
  def withOutputKey(s: Option[String]) = copy(outputKey = s)
}

//TODO Use Numeric instead of Number
case class NumericRangeFacet(fieldName: String, start: Number, end: Number, gap: Number, other: String, outputKey: Option[String] = None) extends RangeFacet[Number, Number] {
  protected def boundAsText(n: Number) = n.toString
  protected def gapAsText(g: Number) = g.toString
  def withOutputKey(s: Option[String]) = copy(outputKey = s)
}
