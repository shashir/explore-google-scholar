import java.net.URL

import scala.concurrent._
import java.io._

/**
 * Created by shashir on 4/10/15.
 */
final case class Article(
  title: String,
  authors: Option[String],
  extract: Option[String],
  pdfUrl: String
)
