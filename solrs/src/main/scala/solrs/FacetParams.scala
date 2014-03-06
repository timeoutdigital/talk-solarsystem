package com.timeout
package solrs

import org.apache.solr.common.params.FacetParams._

case class FacetParams(fields: Seq[FacetField],
                       ranges: Seq[RangeFacet[_, _]],
                       missing: Boolean = false,
                       minCount: Int = 0,
                       limit: Option[Int] = None) extends BaseParams {

  def isEmpty = fields.isEmpty && ranges.isEmpty

  def nonEmpty = fields.nonEmpty || ranges.nonEmpty

  override protected def requiredParams = Seq(
    FACET -> "true",
    FACET_MISSING -> missing.toString,
    FACET_MINCOUNT -> minCount.toString
  )

  override protected def optionalParams = Seq(
    FACET_LIMIT -> limit.map(_.toString)
  )

  override protected def seqParams = Seq(
    FACET_FIELD -> fields.map(_.toString)
  ) ++ ranges.flatMap(_.toSolrQuery)

}
