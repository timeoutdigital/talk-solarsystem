/*
 * Copyright (C) 2013 Time Out Group Limited
 * All rights reserved.
 */

package com.timeout
package solrs

import org.apache.solr.common.params.SpatialParams

import common._

case class GeoField(value: String, tpe: GeoFieldType)

sealed trait GeoFieldType extends BaseEnum {
  def apply(filter: GeoFilter): GeoFilter.Result
}
object GeoFieldType extends EnumContainer[GeoFieldType] {

  object PrefixTree extends GeoFieldType {

    def apply(geo: GeoFilter) = {
      GeoFilter.Result(
        sortFieldName = "query({!score=distance filter=false v=$spatial_filter})",
        filterQueries = Seq("{! v=$spatial_filter}"),
        queryParams = Seq(
          ("spatial_filter", s"${geo.field.value}:" + "\"" + s"Intersects(Circle(${geo.latitude},${geo.longitude} d=${geo.distanceInDegrees}))" + "\"")
        )
      )
    }

  }
  object LatLon extends GeoFieldType {
    import SpatialParams._
    def apply(geo: GeoFilter) = GeoFilter.Result(
      sortFieldName = "geodist()",
      filterQueries = Seq("{!geofilt}"),
      queryParams = Seq(
        FIELD -> geo.field.value,
        POINT -> s"${geo.latitude}, ${geo.longitude}",
        DISTANCE -> geo.distance.toString
      )
    )
  }

  lazy val values: Set[GeoFieldType] = SealedObjects[GeoFieldType]
}
