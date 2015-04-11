package scholar

/**
 * Created by shashir on 4/10/15.
 */
final case class Article(
  title: String,
  authors: Option[String],
  extract: Option[String],
  pdfUrl: String
)
