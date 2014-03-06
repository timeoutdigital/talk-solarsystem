/*
 * Copyright (C) 2012 Time Out Group Limited
 * All rights reserved.
 */

package com.timeout
package http

import java.util.concurrent.{TimeUnit, Executors}

import scala.concurrent.{ExecutionContextExecutorService, ExecutionContext, Future}

import org.apache.http.client.ResponseHandler
import org.apache.http.impl.conn.PoolingHttpClientConnectionManager
import org.apache.http.conn.ssl.{SSLContexts, SSLConnectionSocketFactory, BrowserCompatHostnameVerifier, AllowAllHostnameVerifier}
import org.apache.http.HttpResponse
import org.apache.http.client.methods.HttpUriRequest
import org.apache.http.impl.client.{CloseableHttpClient, HttpClients}
import org.apache.http.config.RegistryBuilder
import org.apache.http.conn.socket.{ConnectionSocketFactory, PlainConnectionSocketFactory}
import org.apache.http.client.config.RequestConfig

import common._

class HttpClientWrapper(allowAllHostNameVerifier: Boolean = false) {

  private val MaxConnections = 100
  private val MaxPerRoute = MaxConnections / 2

  val requestConfig = RequestConfig.custom
    .setSocketTimeout(20 * 1000)
    .setConnectTimeout(10 * 1000)
    .build

  val httpClient: CloseableHttpClient = {

    val socketFactoryRegistry = {
      val rb = RegistryBuilder.create[ConnectionSocketFactory]
      rb.register("http", PlainConnectionSocketFactory.INSTANCE)
      val hostnameVerifier = if (allowAllHostNameVerifier) new AllowAllHostnameVerifier else new BrowserCompatHostnameVerifier
      rb.register("https", new SSLConnectionSocketFactory(SSLContexts.createDefault, hostnameVerifier))
      rb.build
    }

    val connectionManager = new PoolingHttpClientConnectionManager(socketFactoryRegistry)
    connectionManager.setMaxTotal(MaxConnections)
    connectionManager.setDefaultMaxPerRoute(MaxPerRoute)

    HttpClients.custom
      .setConnectionManager(connectionManager)
      .setDefaultRequestConfig(requestConfig)
      .build

  }

  private implicit val _executionContext: ExecutionContextExecutorService =
    ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(MaxConnections, new NamedThreadFactory("httpclient")))

  /**
   * We expose the executionContext so that users of this httpClient can also share the executionContext
   * even if they have no control over the calls themselves. For example, `DefaultSolrClient` wraps
   * calls to the Solrj library in a Future and passes a shared HttpClient instance, but the calls
   * to HttpClient are done by Solrj.
   */
  def executionContext: ExecutionContext = _executionContext

  def execute(request: HttpUriRequest): Future[HttpResponse] = httpClient.asyncExecute(request)

  def execute[T](request: HttpUriRequest, responseHandler: ResponseHandler[T]): Future[T] =
    httpClient.asyncExecute(request, responseHandler)

  def dispose {
    _executionContext.shutdown
    _executionContext.awaitTermination(30, TimeUnit.SECONDS)
    httpClient.close
  }

}
