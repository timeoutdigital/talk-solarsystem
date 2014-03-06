package com.timeout
package http

import scala.concurrent.{ExecutionContext, Future}

import java.io._
import java.net.URI

import org.apache.http.HttpResponse
import org.apache.http.client.{ResponseHandler, HttpClient}
import org.apache.http.client.methods.{HttpGet, HttpPost, HttpUriRequest}
import org.apache.http.util.EntityUtils

import common._

class RichHttpClient(val httpClient: HttpClient) extends AnyVal {

  def download(uri: URI, os: => OutputStream): Option[Throwable] = {
    val response = httpClient.execute(new HttpGet(uri))

    if (response.getStatusLine.getStatusCode >= 300) {
      EntityUtils.consume(response.getEntity)
      Some(new Exception(s"Server returned ${response.getStatusLine.getStatusCode} http status code"))
    }

    Validation.allCatch(using(os)(response.getEntity.writeTo(_))).fail.toOption
  }

  def download(uri: URI, file: File): Option[Throwable] = download(uri, new FileOutputStream(file))

  def safeExecute[T](request: HttpUriRequest, responseHandler: ResponseHandler[T]): Validation[Throwable, T] =
    Validation.allCatch(httpClient.execute(request, responseHandler))

  def asyncExecute(request: HttpUriRequest)(implicit ec: ExecutionContext): Future[HttpResponse] = Future {
    httpClient.execute(request)
  }

  def asyncExecute[T](request: HttpUriRequest, responseHandler: ResponseHandler[T])(implicit ec: ExecutionContext): Future[T] = Future {
    httpClient.execute(request, responseHandler)
  }

}
