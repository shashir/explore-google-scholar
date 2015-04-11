import java.io.File

import scala.concurrent.Future
import org.jsoup.Jsoup
import org.jsoup.nodes._
import scala.collection._
import scala.collection.JavaConverters._
import org.apache.pdfbox.pdmodel.PDDocument
import org.apache.pdfbox.util.PDFTextStripper

import java.net.{URL, URLEncoder}
import scala.concurrent.ExecutionContext.Implicits.global

final case class ScholarQuery(eeCreator: ExtendedExtractCreator) {
  private val extendedExtractsCache: mutable.Map[String, Future[String]] = mutable.Map()

  def apply(
    query: String,
    numRequested: Int,
    numToQuery: Int = 100
    ): Stream[Article] = {
    ScholarQuery.query(eeCreator, extendedExtractsCache)(query, numRequested, numToQuery)
  }

  def getExtendedExtract(title: String): Future[String] = {
    return extendedExtractsCache.getOrElse(title, Future(""))
  }
}

object ScholarQuery {
  val SCHOLAR_QUERY_FORMAT: String = "https://scholar.google.com/scholar?hl=en&q=%s&start=%d&num=%d"
  val ABUSE_EXCEPTION: String = "&google_abuse=GOOGLE_ABUSE_EXEMPTION%3DID%3Db2876d13b3004eae:TM%3D14" +
    "28766803:C%3Dc:IP%3D199.87.86.249-:S%3DAPGng0sE2FE4fMSj1LyumPR" +
    "HJDuZwfPPdw%3B+path%3D/%3B+domain%3Dgoogle.com%3B+expires%3DSat,+11-Apr-2015+18:40:03+GMT"
  val USER_AGENT: String = "Mozilla/5.0 (X11; U; Linux x86_64; it-it) AppleWebKit/534.26+ " +
    "(KHTML, like Gecko) Ubuntu/11.04 Epiphany/2.30.6"
  val RESULT_SELECTOR: String = "div.gs_r"
  val TITLE_SELECTOR: String = "div.gs_ri > h3.gs_rt > a"
  val AUTHOR_SELECTOR: String = "div.gs_ri > div.gs_a"
  val EXTRACT_SELECTOR: String = "div.gs_ri > div.gs_rs"
  val PDF_URL_SELECTOR: String = "div.gs_ttss > a"
  val HTML_SWITCH_SELECTOR: String = "span.gs_ggsL > span.gs_ctg2"
  val RESULTS_PER_PAGE: Int = 10


  private def query(eeCreator: ExtendedExtractCreator, extendedExtractsCache: mutable.Map[String, Future[String]])(
    query: String,
    numRequested: Int,
    numToQuery: Int = 100
  ): Stream[Article] = {
    (0 until numToQuery by RESULTS_PER_PAGE).toStream.flatMap { start: Int =>
      Jsoup.connect(SCHOLAR_QUERY_FORMAT.format(
        URLEncoder.encode(query, "UTF-8"),
        start,
        RESULTS_PER_PAGE
      ) + ABUSE_EXCEPTION).userAgent(USER_AGENT)
        .get
        .select(RESULT_SELECTOR).asScala.view
        .flatMap { result: Element =>
        (
          result.select(TITLE_SELECTOR).asScala.headOption,
          result.select(PDF_URL_SELECTOR).asScala.headOption
          ) match {
          case (Some(titleElement: Element), Some(pdfUrlElement: Element)) => {
            if (pdfUrlElement.select(HTML_SWITCH_SELECTOR).text().contains("HTML")) {
              None
            } else {
              val article: Article = Article(
                titleElement.text,
                result.select(AUTHOR_SELECTOR).asScala.headOption.map(_.text),
                result.select(EXTRACT_SELECTOR).asScala.headOption.map(_.text),
                pdfUrlElement.attr("href")
              )
              extendedExtractsCache.getOrElseUpdate(
                article.title,
                Future(eeCreator(article, downloadPdfToString(article.pdfUrl)))
              )
              Some(article)
            }
          }
          case _ => None
        }
      }
    }.take(numRequested).toStream
  }

  def downloadPdfToString(url: String): String = {
    try {
      val doc = PDDocument.load(new URL(url))
      val stripper = new PDFTextStripper
      return stripper.getText(doc).replaceAll("[\\r\\n]", " ").replace("  ", " ")
    } catch {
      case _ => return "Unable to download PDF."
    }
  }
}