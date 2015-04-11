package scholar

import java.net.{URL, URLEncoder}

import extended_extract.ExtendedExtractCreator
import org.apache.pdfbox.pdmodel.PDDocument
import org.apache.pdfbox.util.PDFTextStripper
import org.jsoup.Jsoup
import org.jsoup.nodes._
import scholar.Article

import scala.collection.JavaConverters._
import scala.collection._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

final case class ScholarQuery(eeCreator: ExtendedExtractCreator) {
  private val extendedExtractsCache: mutable.Map[String, Future[String]] = mutable.Map()

  def apply(
    query: String,
    numRequested: Int,
    numToQuery: Int = 100
    ): Seq[Article] = {
    ScholarQuery.query(eeCreator, extendedExtractsCache)(query, numRequested, numToQuery)
  }

  def getExtendedExtract(title: String): Future[String] = {
    return extendedExtractsCache.getOrElse(title, Future(""))
  }
}

object ScholarQuery {
  val SCHOLAR_QUERY_FORMAT: String = "https://scholar.google.com/scholar?hl=en&q=%s&start=%d&num=%d"
  val ABUSE_EXCEPTION: String = "&google_abuse=GOOGLE_ABUSE_EXEMPTION%3DID%3Dc42842052016be94:TM%3D1428789" +
    "995:C%3Dc:IP%3D199.87.86.249-:S%3DAPGng0uFPHtR0jgRoajFWpIKdFrExLwYzg%3B+path%3D" +
    "/%3B+domain%3Dgoogle.com%3B+expires%3DSun,+12-Apr-2015+01:06:35+GMT"
  val USER_AGENT: String = "Mozilla/5.0 (compatible; Googlebot/2.1; +http://www.google.com/bot.html)"
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
  ): Seq[Article] = {
    (0 until numToQuery by RESULTS_PER_PAGE).view.flatMap { start: Int =>
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
    }.take(numRequested).view
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