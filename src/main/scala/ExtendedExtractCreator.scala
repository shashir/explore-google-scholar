import epic.parser._
import epic.preprocess.MLSentenceSegmenter

import scala.collection.parallel.ParMap

trait ExtendedExtractCreator {
  def apply(article: Article, articleText: String): String
}

object WholeArticle extends ExtendedExtractCreator {
  def apply(article: Article, articleText: String): String = {
    return articleText.take(2000)
  }
}

class FuzzyMatchString(string: String) {
  private def diff(a: String, b: String): Int = {
    val lenDiff: Int = math.abs(a.length - b.length)
    return a.zip(b).foldLeft(lenDiff) { (agg, next: (Char, Char)) => if (next._1 != next._2) agg + 1 else agg }
  }
  def fuzzyIndexOf(key: String, threshold: Int = 10): Int = {
    string.sliding(key.length).zipWithIndex.foreach { case (slice: String, index: Int) =>
      if (diff(slice, key) <= threshold) {
        return index
      }
    }
    return -1
  }
  def fuzzyEquals(key: String, threshold: Int = 10): Boolean = {
    return diff(string, key) <= threshold
  }
}

object ApproximateAbstract extends ExtendedExtractCreator {
  implicit def fuzzyMatchingString(string: String) = new FuzzyMatchString(string)
  def apply(article: Article, articleText: String): String = {
    val indexOfAbstract: Int = articleText.indexOf("Abstract")
    if (indexOfAbstract > 0) {
      return articleText.substring(indexOfAbstract + 8).take(2000).trim
    }
    if (!article.extract.isEmpty) {
      val indexOfExtract: Int = articleText.fuzzyIndexOf(
        article.extract.get
          .replace("... ", "")
          .replace("Abstract", "")
          .take(100).trim, 40)
      if (indexOfExtract > 0) {
        return articleText.substring(indexOfExtract).take(2000).trim
      }
    }

    return ""
  }
}

case class SentenceBasedExtractCreator(
  weightedStrategies: ParMap[SentenceBasedExtractCreatorStrategy, Double]
) extends ExtendedExtractCreator {
  import SentenceBasedExtractCreator._

  def apply(article: Article, articleText: String): String = {
    sentences(articleText).map(tokens).sliding(NUM_SENTENCES).foldLeft(DensestChunk("", 0.0)) {
      case (densest: DensestChunk, nextSentences: IndexedSeq[IndexedSeq[String]]) =>
        val density: Double = weightedStrategies.map { case (strategy, weight) =>
          weight *  strategy(article, nextSentences)
        }.sum
        if (density > densest.density) {
          DensestChunk(
            buildSentence(nextSentences),
            density
          )
        } else {
          densest
        }
    }.chunk
  }
}

object SentenceBasedExtractCreator {
  val NUM_SENTENCES: Int = 10
  final case class DensestChunk(chunk: String, density: Double)
  val sentenceSplitter = MLSentenceSegmenter.bundled().get
  val tokenizer = new epic.preprocess.TreebankTokenizer()
  // val parser = epic.models.ParserSelector.loadParser("en").get
  def sentences(text: String): IndexedSeq[String] = {
    sentenceSplitter(text).map(_.trim).toIndexedSeq
  }
  def tokens(sentence: String): IndexedSeq[String] = {
    tokenizer(sentence).map(_.trim)
  }
  def buildSentence(tokensBySentence: IndexedSeq[IndexedSeq[String]]): String = {
    tokensBySentence.map(_.mkString(" ")).mkString("")
      .replace(" .", ". ").replace(" ,", ", ").replace("  ", " ").replace("( ", "(").replace(" )", ")")
  }
}

trait SentenceBasedExtractCreatorStrategy {
  def apply(article: Article, tokensBySentence: IndexedSeq[IndexedSeq[String]]): Double
}

object DenseWordsSentences extends SentenceBasedExtractCreatorStrategy {
  import SentenceBasedExtractCreator._
  def apply(article: Article, tokensBySentence: IndexedSeq[IndexedSeq[String]]): Double = {
    val allTokens: Seq[String] = tokensBySentence.flatten
    return allTokens.size / NUM_SENTENCES.toDouble
  }
}

object DenseCharactersSentences extends SentenceBasedExtractCreatorStrategy {
  def apply(article: Article, tokensBySentence: IndexedSeq[IndexedSeq[String]]): Double = {
        val allTokens: Seq[String] = tokensBySentence.flatten
        return allTokens.map(_.size).sum / allTokens.size.toDouble
  }
}

object TitlePacked extends SentenceBasedExtractCreatorStrategy {
  import SentenceBasedExtractCreator._
  def apply(article: Article, tokensBySentence: IndexedSeq[IndexedSeq[String]]): Double = {
    val titleTokens: Set[String] = tokens(article.title).toSet
    val allTokens: Seq[String] = tokensBySentence.flatten
    return allTokens.filter(titleTokens.contains(_)).size / NUM_SENTENCES.toDouble
  }
}

object FewestNumerics extends SentenceBasedExtractCreatorStrategy {
  def apply(article: Article, tokensBySentence: IndexedSeq[IndexedSeq[String]]): Double = {
    val allTokens: Seq[String] = tokensBySentence.flatten
    return allTokens.map(_.filterNot { c => c.asDigit < 10 }.size).sum / (allTokens.map(_.size).sum + 1).toDouble
  }
}

object FewestTableAndFigures extends SentenceBasedExtractCreatorStrategy {
  implicit def fuzzyMatchingString(string: String) = new FuzzyMatchString(string)
  val stopWords = Set("Table", "TABLE", "Figure", "FIGURE")
  import SentenceBasedExtractCreator._
  def apply(article: Article, tokensBySentence: IndexedSeq[IndexedSeq[String]]): Double = {
    return tokensBySentence.filterNot(
      _.exists(a => stopWords.exists(s => s.fuzzyEquals(a.trim(), 2)))).size / NUM_SENTENCES.toDouble
  }
}