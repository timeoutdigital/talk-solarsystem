package com.timeout
package solrs

import org.apache.solr.common.params.SpellingParams._

case class SpellingParams(q: Option[String],
                          count: Int,
                          accuracy: Double,
                          extendedResults: Boolean = false,
                          collate: Boolean,
                          collateExtendedResults: Boolean = false,
                          maxCollations: Int = 1,
                          maxCollationTries: Int = 0) extends BaseParams {

  override protected def requiredParams = Seq(
    "spellcheck" -> "true",
    SPELLCHECK_COUNT -> count.toString,
    SPELLCHECK_COLLATE -> collate.toString,
    SPELLCHECK_ACCURACY -> accuracy.toString,
    SPELLCHECK_EXTENDED_RESULTS -> extendedResults.toString,
    SPELLCHECK_COLLATE_EXTENDED_RESULTS -> collateExtendedResults.toString,
    SPELLCHECK_MAX_COLLATIONS -> maxCollations.toString,
    SPELLCHECK_MAX_COLLATION_TRIES -> maxCollationTries.toString
  )

  override protected def optionalParams = Seq(
    SPELLCHECK_Q -> q
  )
  
}
