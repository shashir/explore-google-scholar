import java.net.URLDecoder
import akka.actor.{ActorSystem, Props}
import akka.io.IO
import extended_extract._
import scholar.{ScholarQuery, Article}
import service.CORSSupport
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

object Boot extends App {
  implicit val system = ActorSystem("actor-system")
  val scholarActor = system.actorOf(Props[ScholarActor])
  implicit val timeout = Timeout(5.seconds)
  IO(Http) ? Http.Bind(scholarActor, interface = "0.0.0.0", port = args(0).toInt)
}

class ScholarActor extends Actor with HttpService with CORSSupport {
  import ScholarActor._
  def receive  = runRoute(resultsRoute ~ extractRoute)

  def actorRefFactory = context

  val resultsRoute: Route = cors { get {
    path("results") {
      parameter('q, 'n) { (encodedQuery, num) =>
        val decodedQuery: String = replacer(encodedQuery)
        respondWithMediaType(`application/json`) {
          complete(queryEngine(decodedQuery, num.toInt, num.toInt * 4))
        }
      }
    }
  }}

  val extractRoute: Route =  cors { get {
    path("extendedExtract") {
      parameter('t) { encodedTitle =>
        val decodedTitle: String = replacer(encodedTitle)
        respondWithMediaType(`application/json`) {
          complete(ExtractData(Await.result(queryEngine.getExtendedExtract(decodedTitle), 30.seconds)))
        }
      }
    }
  }}
}

object ScholarActor extends DefaultJsonProtocol with SprayJsonSupport {
  implicit val articleFormat: RootJsonFormat[Article] = jsonFormat4(Article)
  implicit val extractDataFormat: RootJsonFormat[ExtractData] = jsonFormat1(ExtractData)
  final case class ExtractData(data: String)

  val queryEngine = ScholarQuery(SentenceBasedExtractCreator(
    ParMap(
      FewestTableAndFigures -> 10000.0,
      FewestNumerics -> 2000.0,
      TitlePacked -> 1000.0,
      DenseCharactersSentences -> 1.0,
      DenseWordsSentences -> 100
    )
  ))

  def replacer(out: String): String = {
    return URLDecoder.decode(out.replaceAll("%(?![0-9a-fA-F]{2})", "%25").replaceAll("\\+", "%2B"), "UTF-8");
  }
}