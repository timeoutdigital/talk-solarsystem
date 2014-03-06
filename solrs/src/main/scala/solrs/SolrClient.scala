/*
 * Copyright (C) 2013 Time Out Group Limited
 * All rights reserved.
 */

package com.timeout
package solrs

import scala.collection.JavaConverters._
import scala.concurrent.{ExecutionContext, Future}

import org.apache.solr.client.solrj.impl.{BinaryResponseParser, HttpSolrServer, BinaryRequestWriter, LBHttpSolrServer}
import org.apache.solr.client.solrj.SolrServer
import org.apache.solr.client.solrj.{SolrQuery, SolrRequest}
import org.apache.solr.client.solrj.request.{UpdateRequest, QueryRequest}
import org.apache.solr.client.solrj.response.{FacetField => JFacetField, RangeFacet => JRangeFacet, _}
import org.slf4j.LoggerFactory

import common._
import http.HttpClientWrapper
import solrs.SolrClient.CommitParams

trait SolrClient {

  private lazy val logger = LoggerFactory.getLogger(getClass)

  import SolrClient._

  protected def readServer: SolrServer
  protected def executionContext: ExecutionContext

  private def maybeLogQuery(query: SolrQuery): Unit =
    if (logger.isDebugEnabled) logger.debug("Solr query: " + query.toString)

  def run[T](query: Query)(mapper: OutputDocument => T = identity[OutputDocument] _): Future[Response[T]] = Future {
    val solrQuery = query.toSolrQuery
    maybeLogQuery(solrQuery)
    val queryRequest = new QueryRequest(solrQuery, SolrRequest.METHOD.GET)
    Response(queryRequest.process(readServer), query.page)(mapper)
  }(executionContext)

}

trait SolrWriteClient extends SolrClient {

  import SolrClient._

  protected def writeServer: SolrServer

  @inline
  private def runUpdate(f: UpdateRequest => UpdateRequest): Future[UpdateResponse] = Future {
    val updateRequest = new UpdateRequest("/update/javabin")
    f(updateRequest)
    updateRequest.process(writeServer)
  }(executionContext)

  //TODO Replace UpdateResponse with a Scala implementation
  def update(doc: InputDocument): Future[UpdateResponse] = update(Seq(doc))
  def update(docs: Seq[InputDocument]): Future[UpdateResponse] = runUpdate(_.add(docs.map(_.asJava).asJava))

  //TODO Return something more appropriate for a delete call
  def delete(id: String): Future[UpdateResponse] = delete(Seq(id))
  def delete(ids: Seq[String]): Future[UpdateResponse] = runUpdate(_.deleteById(ids.asJava))

  //TODO Return something more appropriate for a delete call
  def deleteByQuery(query: String): Future[UpdateResponse] = runUpdate(_.deleteByQuery(query))

  //TODO Return something more appropriate for a commit call
  def commit(params: CommitParams = CommitParams()): Future[UpdateResponse] = Future {
    import params._
    writeServer.commit(waitFlush, waitServer, softCommit)
  }(executionContext)

}

object SolrClient {

  case class CommitParams(waitFlush: Boolean = true, waitServer: Boolean = true, softCommit: Boolean = false) //TODO Return something more appropriate for a delete call)

  object Response {

    def apply[T](queryResponse: QueryResponse, page: Page)(documentsMapper: OutputDocument => T = identity[OutputDocument] _): Response[T] = {

      def asScala[T](s: java.util.List[T]) = Option(s).fold(Seq.empty[T])(_.asScala)

      val total = Option(queryResponse.getResults).fold(0L)(_.getNumFound)

      val spellCheck = Option(queryResponse.getSpellCheckResponse)
      val facetFields = asScala(queryResponse.getFacetFields)
      val facetRanges = asScala(queryResponse.getFacetRanges)
      val group = Option(queryResponse.getGroupResponse)

      val documents = asScala(queryResponse.getResults).map(doc => documentsMapper(new OutputDocument(doc)))

      Response(documents, page, total, spellCheck, facetFields, facetRanges, group)

    }

  }

  case class Response[T](
    documents: Seq[T],
    page: Page,
    total: Long,
    spellCheck: Option[SpellCheckResponse] = None,
    facetFields: Seq[JFacetField] = Seq.empty,
    facetRanges: Seq[JRangeFacet[_, _]] = Seq.empty,
    group: Option[GroupResponse] = None
  ) {

    def isEmpty: Boolean = documents.isEmpty

    def nonEmpty: Boolean = documents.nonEmpty

  }

}

trait BaseSolrClients[SC <: SolrClient] {
  def clientByCore: Map[String, SC]
  def get(coreName: String): Option[SC] = clientByCore.get(coreName)
}

class SolrClients(val clientByCore: Map[String, SolrClient]) extends BaseSolrClients[SolrClient]

class SolrWriteClients(val clientByCore: Map[String, SolrWriteClient]) extends BaseSolrClients[SolrWriteClient] {
  def commit(params: CommitParams = CommitParams()) {
    clientByCore.values.foreach(_.commit(params))
  }
}

final class DefaultSolrClient(httpClient: HttpClientWrapper, baseReadUrls: Seq[String], baseWriteUrl: String) extends SolrWriteClient {
  require(baseReadUrls.nonEmpty, "baseReadUrls should have at least one element")

  protected def executionContext = httpClient.executionContext

  protected def createServer(url: String): SolrServer = {
    val s = new HttpSolrServer(url, httpClient.httpClient)
    s.setRequestWriter(new BinaryRequestWriter)
    s
  }

  /* No need to shutdown the servers as `HttpClientWrapper` will be disposed when we dispose the Injector */

  //TODO Consider using `ConcurrentUpdateSolrServer`
  protected val writeServer = createServer(baseWriteUrl)

  protected val readServer = {
    if (baseReadUrls.size == 1) createServer(baseReadUrls.head)
    else {
      val responseParser = new BinaryResponseParser
      new LBHttpSolrServer(httpClient.httpClient, responseParser, baseReadUrls: _*) {
        override def makeServer(server: String): HttpSolrServer = {
          val s = new HttpSolrServer(server, httpClient.httpClient, responseParser)
          s.setRequestWriter(new BinaryRequestWriter)
          s
        }
      }
    }
  }

  def dispose {
    writeServer.shutdown
    readServer.shutdown
  }

}

