package service

import java.net.URLDecoder
import akka.actor.{ActorSystem, Props}
import akka.io.IO
import extended_extract._
import scholar.{ScholarQuery, Article}
import spray.can.Http
import spray.http.MediaTypes
import spray.httpx.SprayJsonSupport
import spray.routing.HttpService
import akka.pattern.ask
import akka.util.Timeout
import scala.collection.parallel.ParMap
import scala.concurrent.Await
import scala.concurrent.duration._
import spray.json.{RootJsonFormat, DefaultJsonProtocol}
import MediaTypes._
import akka.actor.Actor
import spray.routing._

/**
 * Web service for querying Google Scholar results and extended extracts for the articles.
 * Always query Google Scholar results first and then look up extended extracts with the article titles.
 * The extracts take a while to compute. This computation starts asynchronously for each article from the time
 * of the Google Scholar query. The computation involves downloading and parsing the article's PDF according to the
 * desired strategy. The following strategies are available:
 *
 * <ul>
 *  <li>SpecialSauce -- a weighted combination of packing title words, removing captions, reducing numerics,
 *     and maximizing density of the sentences (number of characters as well as number of words).</li>
 *  <li>TitlePacked -- find the sentences which use title words the most.</li>
 *  <li>Dense -- find the sentences with the most characters.</li>
 *  <li>Wordy -- find the sentences with the most words.</li>
 *  <li>Abstract -- attempt to locate the abstract using queues.</li>
 * </ul>
 *
 * This application requires the port to bind to as the first program argument. E.g.
 * {{{./Boot 8080}}}
 */
object Boot extends App {
  implicit val system = ActorSystem("actor-system")
  val scholarActor = system.actorOf(Props[ScholarActor])
  implicit val timeout = Timeout(5.seconds)

  // Start service on port.
  IO(Http) ? Http.Bind(scholarActor, interface = "0.0.0.0", port = args(0).toInt)
}

/**
 * Configure routes and query results from the scholar API.
 */
class ScholarActor extends Actor with HttpService with CORSSupport {
  import ScholarActor._
  def receive  = runRoute(resultsRoute ~ extractRoute)
  def actorRefFactory = context

  /**
   * Use path {{{/results?q=query&n=num&s=strategy}}} to query Google Scholar for results to the query for the number
   * requested (we only keep those results with PDFs -- but we query up to 10 times as many results in hopes of finding
   * the PDFs). We use the specified strategy to compute the extended extract with this stored away in a cache for
   * a later query.
   */
  val resultsRoute: Route = cors { get {
    path("results") {
      parameter('q, 'n, 's) { case (encodedQuery: String, num: String, strategy: String) =>
        val decodedQuery: String = urlDecode(encodedQuery)
        val queryEngine: ScholarQuery = STRATEGIES.get(strategy).get
        respondWithMediaType(`application/json`) {
          complete(queryEngine(decodedQuery, num.toInt, num.toInt * 10))
        }
      }
    }
  }}

  /**
   * Use path {{{/extendedExtract?t=title&s=strategy}}} to get from cache
   * the extended extract for the specified title computed with the specified strategy. Note that the title must have
   * been queried from Google Scholar with the same strategy with the previous route (resultsRoute).
   */
  val extractRoute: Route =  cors { get {
    path("extendedExtract") {
      parameter('t, 's) { case (encodedTitle: String, strategy: String) =>
        val decodedTitle: String = urlDecode(encodedTitle)
        val queryEngine: ScholarQuery = STRATEGIES.get(strategy).get
        respondWithMediaType(`application/json`) {
          complete(ExtractData(Await.result(queryEngine.getExtendedExtract(decodedTitle), 30.seconds)))
        }
      }
    }
  }}
}

/**
 * Companion object to the routes containing Json conversion and definitions for the available strategies.
 */
object ScholarActor extends DefaultJsonProtocol with SprayJsonSupport {
  // Json marshalling implicits.
  implicit val articleFormat: RootJsonFormat[Article] = jsonFormat4(Article)
  implicit val extractDataFormat: RootJsonFormat[ExtractData] = jsonFormat1(ExtractData)

  // Wrapper for extended extract data.
  final case class ExtractData(data: String)

  // Map of available strategies.
  val STRATEGIES: Map[String, ScholarQuery] = Map(
    // A weighted combination strategy which uses several substrategies to score sentences.
    "SpecialSauce" -> ScholarQuery(SentenceBasedExtractCreator(
      ParMap(
        FewestTableAndFigures -> 10000.0,
        FewestNumerics -> 2000.0,
        TitlePacked -> 1000.0,
        DenseCharactersSentences -> 1.0,
        DenseWordsSentences -> 100
      )
    )),
    // Find sentences which use the most title words.
    "TitlePacked" -> ScholarQuery(SentenceBasedExtractCreator(ParMap(TitlePacked -> 1.0))),
    // Find sentences with the most characters.
    "Dense" -> ScholarQuery(SentenceBasedExtractCreator(ParMap(DenseCharactersSentences -> 1.0))),
    // Find sentences with the most words.
    "Wordy" -> ScholarQuery(SentenceBasedExtractCreator(ParMap(DenseWordsSentences -> 1.0))),
    // Approximately locate the abstract if possible.
    "Abstract" -> ScholarQuery(ApproximateAbstract)
  )

  // URL decoder.
  def urlDecode(out: String): String = {
    return URLDecoder.decode(out.replaceAll("%(?![0-9a-fA-F]{2})", "%25").replaceAll("\\+", "%2B"), "UTF-8");
  }
}