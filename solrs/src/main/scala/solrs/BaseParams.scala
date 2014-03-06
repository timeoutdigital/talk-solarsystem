package com.timeout
package solrs

trait BaseParams {

  protected def requiredParams: Seq[(String, String)] = Seq.empty
  protected def optionalParams: Seq[(String, Option[String])] = Seq.empty
  protected def seqParams: Seq[(String, Seq[String])] = Seq.empty

  def toQueryParams: Seq[(String, Seq[String])] = {
    val optParams = optionalParams.collect { case (name, Some(value)) => name -> value }
    (requiredParams ++ optParams).map { case (name, value) => name -> Seq(value) } ++ seqParams
  }

}
