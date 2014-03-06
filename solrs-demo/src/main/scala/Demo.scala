/*
 * Copyright (C) 2013 Time Out Group Limited
 * All rights reserved.
 */

package com.timeout
package solrs
package demo

import scala.concurrent.Future
import org.apache.solr.client.solrj.response.UpdateResponse

import common._
import SolrClient._

object Demo {

  object Model {
    sealed trait City extends BaseEnum

    object City extends EnumContainer[City] {
      case object London extends City
      case object NewYork extends City
      lazy val values: Set[City] = SealedObjects[City]
    }
  }

  object Schema {
    import Field._
    import Model._

    // TODO Ideally with a complete ADT integration that could be infered for any `EnumContainer` type.
    implicit val city = Writer.string.comap[City](_.id)

    case object City extends Single[City]       ("city_s")
    case object Name extends Single[String]     ("name_s")
  }

  import Schema._
  import Model.City._

  def query: Query = Query("pizza").filter(City := London)

  def read(client: SolrClient): Future[Response[Option[String]]] = 
    client.run(query)(_.get(Name))

  def write(client: SolrWriteClient): Future[UpdateResponse] = 
    client.update {
      InputDocument(
        Name := "Mario Pizza",
        City := NewYork
      )
    }
}
