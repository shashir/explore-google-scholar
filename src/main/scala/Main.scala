import scala.concurrent.Await
import scala.concurrent.Future
import scala.concurrent.duration._

import scala.concurrent.ExecutionContext.Implicits.global

object Main extends App {

  Await.result(
    Future.sequence(ScholarQuery(ApproximateAbstract)("gvhd", 10, 1000).map {
      a => println(a.pdfUrl); ScholarQuery.getExtendedExtract(a.title)
    }), 30.seconds).zipWithIndex.foreach(println)
}
