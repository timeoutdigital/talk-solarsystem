package com.timeout
package solrs

import org.apache.solr.client.solrj.SolrQuery.SortClause
import org.apache.solr.common.params.GroupParams._

case class GroupParams(fields: Seq[String] = Seq.empty,
                       queries: Seq[String] = Seq.empty,
                       offset: Int = 0,
                       limit: Int = 1,
                       sorts: Seq[SortClause] = Seq.empty,
                       ngroups: Option[Boolean] = None) extends BaseParams {

  override protected def requiredParams = Seq(
    GROUP -> "true",
    GROUP_OFFSET -> offset.toString,
    GROUP_LIMIT -> limit.toString) ++
      sorts.headOption.fold(Seq.empty[(String, String)]) { _ =>
        Seq(GROUP_SORT -> sorts.map(s => s"${s.getItem} ${s.getOrder}").mkString(","))
      } ++
      ngroups.fold(Seq.empty[(String, String)]) { ngroups =>
        Seq(GROUP_TOTAL_COUNT -> ngroups.toString)
      }

  override protected def seqParams = Seq(
    GROUP_FIELD -> fields,
    GROUP_QUERY -> queries
  )

}
