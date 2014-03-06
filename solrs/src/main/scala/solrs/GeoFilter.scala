/*
 * Copyright (C) 2013 Time Out Group Limited
 * All rights reserved.
 */

package com.timeout
package solrs

import com.spatial4j.core.distance.DistanceUtils

import com.timeout.solrs.GeoFilter.Result

case class GeoFilter(field: GeoField,
                     latitude: Double,
                     longitude: Double,
                     distance: Double
) {
  require(distance > 0, s"Solr geofilter distance value $distance must be greater then zero!")

  def apply: Result = field.tpe(this)

  def distanceInDegrees: Double = DistanceUtils.dist2Degrees(distance, DistanceUtils.EARTH_MEAN_RADIUS_KM)

}

object GeoFilter {
  case class Result(sortFieldName: String, filterQueries: Seq[String], queryParams: Seq[(String, String)])
}
