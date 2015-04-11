import scala.concurrent.Future
import org.jsoup.Jsoup
import org.jsoup.nodes._
import scala.collection._
import scala.collection.JavaConverters._
import org.apache.pdfbox.pdmodel.PDDocument
import org.apache.pdfbox.util.PDFTextStripper

import java.net.{URL, URLEncoder}
import scala.concurrent.ExecutionContext.Implicits.global

object ScholarQuery {
  val SCHOLAR_QUERY_FORMAT: String = "https://scholar.google.com/scholar?hl=en&q=%s&num=%d"
  val USER_AGENT: String ="Mozilla/5.0 (Macintosh; Intel Mac OS X 10_9_2) AppleWebKit/537.36 " +
    "(KHTML, like Gecko) Chrome/33.0.1750.152 Safari/537.36"
  val RESULT_SELECTOR: String = "div.gs_r"
  val TITLE_SELECTOR: String = "div.gs_ri > h3.gs_rt > a"
  val AUTHOR_SELECTOR: String = "div.gs_ri > div.gs_a"
  val EXTRACT_SELECTOR: String = "div.gs_ri > div.gs_rs"
  val PDF_URL_SELECTOR: String = "div.gs_ttss > a"
  val HTML_SWITCH_SELECTOR: String = "span.gs_ggsL > span.gs_ctg2"


  val extendedExtractsCache: mutable.Map[String, Future[String]] = mutable.Map()

  def apply(eeCreator: ExtendedExtractCreator)(
    query: String,
    numRequested: Int,
    numToQuery: Int = 100
  ): Seq[Article] = {
    val results: Document = Jsoup.connect(SCHOLAR_QUERY_FORMAT.format(
      URLEncoder.encode(query, "UTF-8"),
      numToQuery
    )).userAgent(USER_AGENT).get

    results.select(RESULT_SELECTOR).asScala.view.flatMap { result =>
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
    }.take(numRequested).toSeq
  }

  def getExtendedExtract(title: String): Future[String] = {
    return extendedExtractsCache.getOrElse(title, Future(""))
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