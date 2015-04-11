import epic.parser._

trait ExtendedExtractCreator {
  def apply(article: Article, articleText: String): String
}

object WholeArticle extends ExtendedExtractCreator {
  def apply(article: Article, articleText: String): String = {
    return articleText.take(2000)
  }
}

object ApproximateAbstract extends ExtendedExtractCreator {
  def apply(article: Article, articleText: String): String = {
    val indexOfAbstract: Int = articleText.indexOf("Abstract")
    if (indexOfAbstract > 0) {
      return articleText.substring(indexOfAbstract).take(2000).trim
    }
    if (!article.extract.isEmpty) {
      val indexOfExtract: Int = articleText.indexOf(
        article.extract.get
          .replace("... ", "")
          .replace("Abstract ", "")
          .substring(0, 20).trim)
      if (indexOfExtract > 0) {
        return articleText.substring(indexOfExtract).take(2000).trim
      }
    }

    return ""
  }
}
