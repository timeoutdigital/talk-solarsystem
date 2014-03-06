/*
 * Copyright (C) 2013 Time Out Group Limited
 * All rights reserved.
 */

package com.timeout
package solrs

import org.apache.solr.client.solrj.SolrQuery.ORDER

import common._

/**
 * Provides methods related to SolrQuery.ORDER. In the future, we should wrap SolrQuery.ORDER fully
 * so that users only have to know about the wrapper instead of both.
 */
object Order {

  def parse(value: String): Validation[String, ORDER] = 
    Option(ORDER.valueOf(value)).toValidation {
      val expectedValues = ORDER.values.map(_.name.quote).mkString(",")
      s"Expected one of $expectedValues. Received $value"
    }

}
