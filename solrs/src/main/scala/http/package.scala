package com.timeout

import org.apache.http.client.HttpClient
import scala.language.implicitConversions

package object http {
  @inline implicit final def toRichHttpClient(httpClient: HttpClient): RichHttpClient = new RichHttpClient(httpClient)
}
