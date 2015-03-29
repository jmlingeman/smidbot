package smidbot


//import scala.collection.parallel.{mutable, ParSeq, ParMap}
import scala.collection.mutable
import scala.io.Source
import scala.util.Random

/**
 * Created by jesse on 3/28/15.
 */
class MarkovChainGeneration(filename: String) {
  val SENTENCE_END = "@@@"
  val SENTENCE_START = "###"
  val random = new Random()

  val wordMap = loadAndParseAlexLog(filename)

  def loadAndParseLog(filename: String) = {

//    val wordMap = new HashMap[String, HashMap[String, Int].withDefaultValue(0)]

    // Last slot is msg slot
    val msgPosition = 6

    val file = Source.fromFile(filename)
    val lines = file.getLines().toList

    println("Building word list")
    val wordLists = lines.par.map { x =>
      val linesp = x.split("\t")
      val msg = linesp.last
//      println(msg)
//      val msg = if(linesp.length == 8) linesp(6) else linesp.slice(6, linesp.length-2).mkString(" ")
      val wl = ("\\b\\w+\\b".r findAllIn msg).sliding(3).toSeq

//      println(wl)

      if(wl.size > 0 && wl.last.size == 3 && wl(0).size == 3)
        wl ++ Seq(Seq(wl.last(1), wl.last(2), SENTENCE_END)) ++ Seq(Seq(SENTENCE_START, wl(0)(0), wl(0)(1)))
      else
        wl
    }.flatten.filter(x => x.size == 3)

    println("Building word map")
    val wordMap = wordLists.groupBy{ x => (x(0), x(1))}.map{ x=> x._1 -> x._2.groupBy(y => y(0)).map(y => y._1 -> y._2.size).toMap}
      .withDefaultValue(Map[String, Int]())
    file.close()

    wordMap
  }

  def loadAndParseAlexLog(filename: String) = {
    val file = Source.fromFile(filename)
    val lines = file.getLines().toList

    println("Building word list")
    val wordMap = new mutable.HashMap[(String, String), mutable.Map[String, Int]]().withDefaultValue(new mutable.HashMap[String, Int]().withDefaultValue(0))
    lines.map { x =>
      val linesp = x.split("\t")
      val msg = linesp.last

      val wl = ("\\b\\w+\\b".r findAllIn msg).sliding(3).toSeq.filter(x => x.size == 3)
      val triples = if(wl.size > 0)
        wl ++ Seq(Seq(wl.last(1), wl.last(2), SENTENCE_END)) ++ Seq(Seq(SENTENCE_START, wl(0)(0), wl(0)(1)))
      else
        wl

      triples.foreach{x =>
//        println(x)
        val tuple = (x(0), x(1))
        if(!wordMap.contains(tuple)) {
          wordMap.put(tuple, new mutable.HashMap[String, Int]().withDefaultValue(0))
        }
        wordMap(tuple).put(x(2), wordMap(tuple)(x(2)) + 1)

//        wordMap((x(0), x(1))).put(x(2), wordMap((x(0), x(1)))(x(2)) + 1)
      }

    }

    file.close()

    wordMap
  }

  def genSentence(w1: String, w2: String, maxChars:Int = 200): String = {

    var sentence = if(w1 != SENTENCE_START) w1 + " " else ""
    var lastToken = w1
    var selectedToken = w2
    while(selectedToken != SENTENCE_END && sentence.size < maxChars) {
//      lastToken = selectedToken
      if(selectedToken != SENTENCE_START)
        sentence = sentence + selectedToken + " "
      val newTokens = wordMap((lastToken, selectedToken))
      if(newTokens.size == 0) {
        selectedToken = SENTENCE_END
      } else {
        lastToken = selectedToken
        val sum = newTokens.map(x => x._2).sum
        val idx = random.nextInt(sum) + 1

//        println(sum, idx, newTokens)

        // Now have to map this idx to a word
        var count = 0
        var token = ""
        var lastIdx = 0
        val t = newTokens.toStream.takeWhile(_ => count < idx).foreach{
          x =>
            count += x._2
//            if(count < idx)
            token = x._1
        }

//        println(count)

        selectedToken = token
      }

    }

    sentence
  }

  def getStartingWords(): (String, String) = {
    val startingWords = wordMap.keys.filter(x => x._1 == SENTENCE_START)
    val words = startingWords.toSeq(random.nextInt(startingWords.size))
    words
  }

  def genRandomSentence(): String = {
    val words = getStartingWords()
    genSentence(words._1, words._2)
  }
}

object MarkovChainGeneration {
  val mcg = new MarkovChainGeneration("geekboy_dump.2015.03.28")

  def main(args: Array[String]) {

    println(mcg.genSentence("holy", "shit"))
    println(mcg.genRandomSentence())
    println(mcg.genRandomSentence())
    println(mcg.genRandomSentence())
    println(mcg.genRandomSentence())

//    val length
  }
}
