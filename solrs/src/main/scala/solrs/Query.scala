/*
 * Copyright (C) 2013 Time Out Group Limited
 * All rights reserved.
 */

package com.timeout
package solrs

import org.apache.solr.client.solrj.SolrQuery
import org.apache.solr.client.solrj.SolrQuery.SortClause

import common._

trait Query {
  def toSolrQuery: SolrQuery 
  def page: Page
}

object Query {
  def apply(q: String = "*:*") = Builder(q)

  case class Builder(
    q: String = "*:*",
    page: Page = Page(),
    filters: Seq[FilterQuery] = Seq.empty,
    // TODO The following fields are not managed using the DSL
    geoFilter: Option[GeoFilter] = None,
    sorts: Seq[SortClause] = Nil,
    params: Seq[BaseParams] = Nil,
    extraParams: Seq[(String, Seq[String])] = Nil,
    fields: Seq[String] = Nil
  ) extends Query {

    def filter[T](f: Field.Set[T])(implicit writer: Field.Writer[T]) = 
      this.copy(filters = FilterQuery(s"${f.name}:${writer.toSolr(f.value)}") +: filters)

    def page(page: Page) = this.copy(page = page)

    def toSolrQuery: SolrQuery = {
      val GeoSort = "geodist()"

      val containsSortByDistance =
        (sorts ++ params.collect { case g: GroupParams => g.sorts }.flatten).exists(_.getItem == GeoSort)

      // TODO Replace with proper Validation
      if (containsSortByDistance) require(geoFilter.isDefined, "Can't sort by distance if `geoFilter` is not defined.")

      val query = new SolrQuery(q)
      query.setStart((page.number - 1) * page.size)
      query.setRows(page.size)
      query.setFilterQueries(filters.map(_.toQueryParams): _*)

      val geoSort = geoFilter.map(_.apply).flatMap { geoResult =>
        query.addFilterQuery(geoResult.filterQueries: _*)
        geoResult.queryParams.foreach { case (n, v) => query.set(n, v) }
        containsSortByDistance.toOption(geoResult.sortFieldName)
      }

      def replaceGeoDist(sortClauses: Seq[SortClause]) =
        sortClauses.map(s => if (s.getItem == GeoSort) new SortClause(geoSort.get, s.getOrder) else s)

      replaceGeoDist(sorts).foreach(query.addSort)

      fields.foreach(query.addField)

      def processParams(params: BaseParams): BaseParams =
        if (containsSortByDistance) {
          params match {
            case gp: GroupParams => gp.copy(sorts = replaceGeoDist(gp.sorts))
            case _ => params
          }
        }
        else params

      (params.flatMap(processParams(_).toQueryParams) ++ extraParams).foreach { case (name, value) => query.set(name, value: _*) }

      query
    }
  }
}
