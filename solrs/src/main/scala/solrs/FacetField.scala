package com.timeout
package solrs

case class FacetField(name: String, outputKey: Option[String] = None, exclude: Option[String] = None) {

  def outputName: String = outputKey.getOrElse(name)

  override def toString: String = {
    val params = Seq("key" -> outputKey, "ex" -> exclude).collect { case (paramName, Some(paramValue)) =>
      s"$paramName=$paramValue"
    }
    if (params.nonEmpty) s"{!${params.mkString(" ")}}$name"
    else name
  }

}
