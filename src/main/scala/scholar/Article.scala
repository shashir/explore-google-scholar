package scholar

/**
 * Record to contain article metadata.
 *
 * @param title of the article.
 * @param authors of the article.
 * @param extract snippet that Google Scholar produces from the article.
 * @param pdfUrl url of the pdf file associated with the article.
 */
final case class Article(
  title: String,
  authors: Option[String],
  extract: Option[String],
  pdfUrl: String
)
