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

/**
 * A class to query Google Scholar and also to apply the desired method for producing
 * an extended extract from the PDF of the article.
 *
 * @param eeCreator the desired method for producing an extended extract from the PDF of the article.
 */
final case class ScholarQuery(eeCreator: ExtendedExtractCreator) {
  /**
   * A cache of previously computed extended extracts keyed by the article title.
   */
  private val extendedExtractsCache: mutable.Map[String, Future[String]] = mutable.Map()

  /**
   * Query specified number of results from Google Scholar using the search query string.
   * Note that are only interested in articles with associated PDFs.
   * We query more articles than we need and filter out those without PDFs.
   * We also launch an asynchronous future to collect the extended extract for each queried article.
   *
   * @param query search query.
   * @param numRequested number of articles with associated PDFs that we would like.
   * @param numToQuery number of articles to query (we filter out those without PDFs).
   * @return sequence of articles with PDFs.
   */
  def apply(
    query: String,
    numRequested: Int,
    numToQuery: Int = 100
  ): Seq[Article] = {
    ScholarQuery.query(eeCreator, extendedExtractsCache)(query, numRequested, numToQuery)
  }

  /**
   * Given an article title, query the cache for the future containing its extended extract.
   * If no extract was store, the an empty string is returned.
   *
   * @param title of the article.
   * @return future containing the extended extract.
   */
  def getExtendedExtract(title: String): Future[String] = {
    return extendedExtractsCache.getOrElse(title, Future(""))
  }
}

/**
 * Companion object containing static fields/methods.
 */
object ScholarQuery {
  /**
   * Google Scholar query format. Must fill in query, start page, and number of articles per page.
   */
  val SCHOLAR_QUERY_FORMAT: String = "https://scholar.google.com/scholar?hl=en&q=%s&start=%d&num=%d"

  val RESULTS_PER_PAGE: Int = 10

  /**
   * Exception string, cookies and user-agent in order to fool Google Scholar into thinking we are human.
   * These string may need to be regenerated every so often.
   */
  val ABUSE_EXCEPTION: String = ""
  val COOKIES: Map[String, String] = Map(
    "GOOGLE_ABUSE_EXEMPTION" -> ("ID=7a5a8657eba5925d:TM=1428823005:C=c:IP=199.87.86.249-:" +
      "S=APGng0uoddqwag2HeDQzvTVMDkBbp84nmw"),
    "PREF" -> "ID=2b22e783bbce62ac:TM=1428823009:LM=1428823009:S=Eft0ytOG3Cr1m4GZ",
    "NID" -> ("67=XiG_KcvuS95VbIxTPAAlw6067ffkeomIaSz0wxAO1IFR8RQ_wFtbZegwod97xAw-oUCzm36E_J7kg0Wditv" +
      "kaVwPbw-OaBckba-ATrq8l3EjQFzrwZ2sQXWzjWwnw31A"),
    "GSP" -> "ID=2b22e783bbce62ac:A=PnmQuA:CPTS=1428823016:LM=1428823016:S=MDVoy-LttMBraw1x"
  )
  val USER_AGENT: String = "Mozilla/5.0 (X11; U; Linux i686; en-US; rv:1.9.0.8) Gecko/20090327 Galeon/2.0.7"

  /**
   * Class selectors to parse the Google Scholar results' page HTML for the title, authors, short extract, and PDF URL.
   */
  val RESULT_SELECTOR: String = "div.gs_r"
  val TITLE_SELECTOR: String = "div.gs_ri > h3.gs_rt > a"
  val AUTHOR_SELECTOR: String = "div.gs_ri > div.gs_a"
  val EXTRACT_SELECTOR: String = "div.gs_ri > div.gs_rs"
  val PDF_URL_SELECTOR: String = "div.gs_ttss > a"
  val HTML_SWITCH_SELECTOR: String = "span.gs_ggsL > span.gs_ctg2"

  /**
   * Query specified number of results from Google Scholar using the search query string.
   * Note that are only interested in articles with associated PDFs.
   * We query more articles than we need and filter out those without PDFs.
   * We also launch an asynchronous future to collect the extended extract for each queried article with the desired
   * method for producing the extended extract. This future is placed in the provided mutable map (a cache).
   *
   * @param eeCreator the desired method for producing an extended extract from the PDF of the article.
   * @param extendedExtractsCache map from title to future containing the computation for getting the extended extract.
   * @param query search query.
   * @param numRequested number of articles with associated PDFs that we would like.
   * @param numToQuery number of articles to query (we filter out those without PDFs).
   * @return sequence of articles with PDFs.
   */
  private def query(eeCreator: ExtendedExtractCreator, extendedExtractsCache: mutable.Map[String, Future[String]])(
    query: String,
    numRequested: Int,
    numToQuery: Int = 100
  ): Seq[Article] = {
    // Google Scholar produces pages of results instead of a contiguous set. Query each page.
    (0 until numToQuery by RESULTS_PER_PAGE).view.flatMap { pageStart: Int =>
      // Query Google Scholar for results.
      Jsoup.connect(SCHOLAR_QUERY_FORMAT.format(
        URLEncoder.encode(query, "UTF-8"),
        pageStart,
        RESULTS_PER_PAGE
      ) + ABUSE_EXCEPTION)
        // Pretend we are not a bot.
        .userAgent(USER_AGENT).referrer("https://scholar.google.com/").cookies(COOKIES.asJava)
        .get
        // Select each result.
        .select(RESULT_SELECTOR).asScala.view
        // For each result element, extract title and PDF URL.
        .flatMap { result: Element =>
        (
          result.select(TITLE_SELECTOR).asScala.headOption,
          result.select(PDF_URL_SELECTOR).asScala.headOption
          ) match {
          // For valid title and PDF URL, produce an Article record.
          case (Some(titleElement: Element), Some(pdfUrlElement: Element)) => {
            if (pdfUrlElement.select(HTML_SWITCH_SELECTOR).text().contains("HTML")) None
            else {
              // Produce article record.
              val article: Article = Article(
                titleElement.text,
                result.select(AUTHOR_SELECTOR).asScala.headOption.map(_.text),
                result.select(EXTRACT_SELECTOR).asScala.headOption.map(_.text),
                pdfUrlElement.attr("href")
              )

              // Create a future to start computing the extended extract from the PDF.
              // Put it in the cache keyed by title.
              extendedExtractsCache.getOrElseUpdate(
                article.title,
                Future {
                  // Download PDF and pass the string to the chosed method for computing the extended extract.
                  eeCreator(article, downloadPdfToString(article.pdfUrl))
                }
              )

              Some(article)
            }
          }
          // If either title or PDF URL is unavailable skip result.
          case _ => None
        }
      }
    }
    // Return the number requested.
    .take(numRequested).view
  }

  /**
   * Download PDF from URL to string.
   *
   * @param url of the PDF.
   * @return PDF contents as string.
   */
  def downloadPdfToString(url: String): String = {
    try {
      val doc = PDDocument.load(new URL(url))
      val stripper = new PDFTextStripper
      // Replace double spaces and carriage returns for easier parsing.
      return stripper.getText(doc).replaceAll("[\\r\\n]", " ").replace("  ", " ")
    } catch {
      case _ => return "Unable to download PDF."
    }
  }
}