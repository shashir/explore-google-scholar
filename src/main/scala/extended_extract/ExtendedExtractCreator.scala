package extended_extract

import epic.preprocess.MLSentenceSegmenter
import scholar.Article

import scala.collection.parallel.ParMap

/**
 * Trait for all method for producing an extended extract from an article's text.
 */
trait ExtendedExtractCreator {
  /**
   * @param article metadata record.
   * @param articleText text from PDF.
   * @return extract.
   */
  def apply(article: Article, articleText: String): String
}

/**
 * Companion for the trait containing a String-pimp class.
 */
object ExtendedExtractCreator {
  val NUM_CHARS_TO_RETURN: Int = 2000;
  val ABSTRACT_STRING: String = "Abstract";

  /**
   * Extend the String type with fuzzy comparison methods.
   *
   * @param string source string.
   */
  class FuzzyMatchString(string: String) {
    /**
     * Align heads of string and compute character mismatches.
     *
     * @param a string
     * @param b string
     * @return number of character mismatches.
     */
    private def diff(a: String, b: String): Int = {
      val lenDiff: Int = math.abs(a.length - b.length)
      return a.zip(b).foldLeft(lenDiff) { (agg, next: (Char, Char)) => if (next._1 != next._2) agg + 1 else agg }
    }

    /**
     * Find the first index in the string where the provided key string differs starting from the index
     * on up to threshold number of characters.
     *
     * @param key to search for in the source string.
     * @param threshold number of character mismatches to tolerate.
     * @return the index of the first match with tolerate character mismatches.
     *   Or -1 if such a substring could not be found.
     */
    def fuzzyIndexOf(key: String, threshold: Int): Int = {
      string.sliding(key.length).zipWithIndex.foreach { case (slice: String, index: Int) =>
        if (diff(slice, key) <= threshold) {
          return index
        }
      }
      return -1
    }

    /**
     * Returns true of the key and the source string differ by up to threshold number of characters.
     *
     * @param key to compare to.
     * @param threshold number of character mismatches to tolerate.
     * @return true iff the key and the source string differ by up to threshold number of characters.
     */
    def fuzzyEquals(key: String, threshold: Int): Boolean = {
      return diff(string, key) <= threshold
    }
  }
}

/**
 * Get the top 2000 characters from the PDF as the extended extract.
 */
object NaivePreamble extends ExtendedExtractCreator {
  import ExtendedExtractCreator._
  override def apply(article: Article, articleText: String): String = {
    return articleText.take(NUM_CHARS_TO_RETURN)
  }
}

/**
 * Find an approximate region of the abstract and return 2000 characters.
 */
object ApproximateAbstract extends ExtendedExtractCreator {
  import ExtendedExtractCreator._
  implicit def fuzzyMatchingString(string: String) = new FuzzyMatchString(string)

  override def apply(article: Article, articleText: String): String = {
    // Match the string "Abstract" and return 2000 characters thence.
    val indexOfAbstract: Int = articleText.indexOf(ABSTRACT_STRING)
    if (indexOfAbstract > 0) {
      return articleText.substring(indexOfAbstract + 8).take(NUM_CHARS_TO_RETURN).trim
    }

    // Match the Google Scholar extract and return 2000 characters thence. "Cheating" by using Google Scholar.
    if (!article.extract.isEmpty) {
      val indexOfExtract: Int = articleText.fuzzyIndexOf(
        article.extract.get
          .replace("... ", "")
          .replace(ABSTRACT_STRING, "")
          .take(100).trim, 40)
      if (indexOfExtract > 0) {
        return articleText.substring(indexOfExtract).take(NUM_CHARS_TO_RETURN).trim
      }
    }

    return "Abstract could not be located."
  }
}

/**
 * Use weighted strategies for scoring contiguous sentences from the article's text.
 * The sequence of sentences with the maximum sum of weighted scores is returned.
 *
 * @param weightedStrategies a parallel map from strategy to associated weights for scoring and then weighting.
 */
case class SentenceBasedExtractCreator(
  weightedStrategies: ParMap[SentenceBasedExtractCreatorStrategy, Double]
) extends ExtendedExtractCreator {
  import extended_extract.SentenceBasedExtractCreator._

  override def apply(article: Article, articleText: String): String = {
    // Consider a sliding window of NUM_SENTENCES sentences.
    // Tokenize the sentences, compute the scores by each specified strategy. Weight and then sum the scores.
    return sentences(articleText).map(tokens).sliding(NUM_SENTENCES).foldLeft(DensestChunk("", 0.0)) {
      case (densest: DensestChunk, nextSentences: IndexedSeq[IndexedSeq[String]]) =>
        // Overall weighted score (the so-called "density").
        val density: Double = weightedStrategies.map { case (strategy, weight) =>
          // Compute weighted score from the strategy.
          weight *  strategy(article, nextSentences)
        }.sum

        // If the weighted score is greater than previously pocketed density, then replace.
        if (density > densest.density) {
          DensestChunk(
            buildSentence(nextSentences),
            density
          )
        } else densest
    }.chunk // return the chunk.
  }
}

/**
 * Companion object with sentence splitter, tokenizer, etc.
 */
object SentenceBasedExtractCreator {
  // Number of sentences to consider in sliding window.
  val NUM_SENTENCES: Int = 10

  // Record for pocketing the "best" window of sentences.
  final case class DensestChunk(chunk: String, density: Double)

  // Splitter.
  val sentenceSplitter = MLSentenceSegmenter.bundled().get

  // Tokenizer
  val tokenizer = new epic.preprocess.TreebankTokenizer()

  // val parser = epic.models.ParserSelector.loadParser("en").get

  /**
   * @param text to split into sentences.
   * @return indexed sequence of sentences.
   */
  def sentences(text: String): IndexedSeq[String] = {
    sentenceSplitter(text).map(_.trim).toIndexedSeq
  }

  /**
   * @param sentence to split into words.
   * @return indexed sequence of words.
   */
  def tokens(sentence: String): IndexedSeq[String] = {
    tokenizer(sentence).map(_.trim)
  }

  /**
   * Produce sentence from sequence of sequence of words. Remove double spaces.
   *
   * @param tokensBySentence sequence of sequence of tokens by sentence.
   * @return concatenated extract of words.
   */
  def buildSentence(tokensBySentence: IndexedSeq[IndexedSeq[String]]): String = {
    tokensBySentence.map(_.mkString(" ")).mkString("")
      .replace(" .", ". ").replace(" ,", ", ").replace("  ", " ").replace("( ", "(").replace(" )", ")")
  }
}

/**
 * Trait for ways to score sentences.
 */
trait SentenceBasedExtractCreatorStrategy {
  /**
   * Method to score a sequence of sentences split into tokens..
   *
   * @param article metadata record.
   * @param tokensBySentence sequence of sequence of tokens by sentence.
   * @return a score how "relevant" this sequence of sentences is.
   */
  def apply(article: Article, tokensBySentence: IndexedSeq[IndexedSeq[String]]): Double
}

/**
 * Compute number of words in given sequence of sentences.
 * Used for finding contiguous sequence of sentences with most words.
 */
object DenseWordsSentences extends SentenceBasedExtractCreatorStrategy {
  import extended_extract.SentenceBasedExtractCreator._

  override def apply(article: Article, tokensBySentence: IndexedSeq[IndexedSeq[String]]): Double = {
    val allTokens: Seq[String] = tokensBySentence.flatten
    return allTokens.size / NUM_SENTENCES.toDouble
  }
}

/**
 * Compute number of letters in given sequence of sentences.
 * Used for finding contiguous sequence of sentences with most letters.
 */
object DenseCharactersSentences extends SentenceBasedExtractCreatorStrategy {
  override def apply(article: Article, tokensBySentence: IndexedSeq[IndexedSeq[String]]): Double = {
    val allTokens: Seq[String] = tokensBySentence.flatten
    return allTokens.map(_.size).sum / allTokens.size.toDouble
  }
}

/**
 * Compute number of title words in given sequence of sentences.
 * Used for finding contiguous sequence of sentences with most title words.
 * The idea is that the title contains the most relevant words to the article.
 * We want to find the sequence of sentences which use the title words a lot.
 */
object TitlePacked extends SentenceBasedExtractCreatorStrategy {
  import extended_extract.SentenceBasedExtractCreator._
  override def apply(article: Article, tokensBySentence: IndexedSeq[IndexedSeq[String]]): Double = {
    val titleTokens: Set[String] = tokens(article.title).toSet
    val allTokens: Seq[String] = tokensBySentence.flatten
    return allTokens.filter(titleTokens.contains(_)).size / NUM_SENTENCES.toDouble
  }
}

/**
 * Compute the number of non-numeric letters in given sequence of sentences.
 * We want to minimize numbers.
 */
object FewestNumerics extends SentenceBasedExtractCreatorStrategy {
  override def apply(article: Article, tokensBySentence: IndexedSeq[IndexedSeq[String]]): Double = {
    val allTokens: Seq[String] = tokensBySentence.flatten
    return allTokens.map(_.filterNot { c => c.asDigit < 10 }.size).sum / (allTokens.map(_.size).sum + 1).toDouble
  }
}

/**
 * Compute number of sentences with no stop words like "Table", "Figure", etc.
 * We don't want to capture captions for tables and figures in the extract if we can help it.
 */
object FewestTableAndFigures extends SentenceBasedExtractCreatorStrategy {
  import ExtendedExtractCreator._
  implicit def fuzzyMatchingString(string: String) = new FuzzyMatchString(string)
  val stopWords = Set("Table", "TABLE", "Figure", "FIGURE")
  import extended_extract.SentenceBasedExtractCreator._
  override def apply(article: Article, tokensBySentence: IndexedSeq[IndexedSeq[String]]): Double = {
    return tokensBySentence.filterNot(
      _.exists(a => stopWords.exists(s => s.fuzzyEquals(a.trim(), 2)))).size / NUM_SENTENCES.toDouble
  }
}