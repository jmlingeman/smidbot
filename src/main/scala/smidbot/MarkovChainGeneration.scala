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

  val dictionary = new mutable.HashMap[String, Int]()
  val revDictionary = new mutable.HashMap[Int, String]()
  var lastKey = 0

  val wordMapBible = loadGutenberg("data/bible.txt")

  val (wordMap, wordMapByNick) = loadAndParseAlexLog(filename)
  //val wordMap3 = loadAndParseAlexLog3(filename)

  def dictionaryLookup(word : String) = {
    if(dictionary.keySet.contains(word)) {
      dictionary(word)
    } else {
      dictionary.put(word, lastKey + 1)
      revDictionary.put(lastKey + 1, word)
      lastKey += 1
      lastKey
    }
  }

  def revDictionaryLookup(i : Int) = {
    revDictionary(i)
  }

  // TODO change all of these to be INT INT maps
  def loadGutenberg(filename: String) = {
    val file = Source.fromFile(filename)

    val lines = file.mkString.split("\\.").toList

    println("Building word list")
    val wordMap = new mutable.HashMap[(Int, Int), mutable.Map[Int, Int]]().withDefaultValue(new mutable.HashMap[Int, Int]().withDefaultValue(0))
    lines.map { x =>
      val msg = if(x.size > 0) x.split(" ").slice(1, x.split(" ").size).mkString(" ") else x

      val wl = ("\\b\\w+\\b".r findAllIn msg).sliding(3).toSeq.filter(x => x.size == 3)
      val triples = if(wl.size > 0)
        wl ++ Seq(Seq(wl.last(1), wl.last(2), SENTENCE_END)) ++ Seq(Seq(SENTENCE_START, wl(0)(0), wl(0)(1)))
      else
        wl

      triples.foreach{x =>
        //        println(x)
        val tuple = (dictionaryLookup(x(0)), dictionaryLookup(x(1)))
        if(!wordMap.contains(tuple)) {
          wordMap.put(tuple, new mutable.HashMap[Int, Int]().withDefaultValue(0))
        }
        wordMap(tuple).put(dictionaryLookup(x(2)), wordMap(tuple)(dictionaryLookup(x(2))) + 1)

        //        wordMap((x(0), x(1))).put(x(2), wordMap((x(0), x(1)))(x(2)) + 1)
      }

    }

    wordMap
  }

  def loadAndParseLog(filename: String) = {

//    val wordMap = new HashMap[String, HashMap[String, Int].withDefaultValue(0)]

    // Last slot is msg slot
    val msgPosition = 6

    val file = Source.fromFile(filename)
    val lines = file.getLines().toList

    println("Building word list")
    val wordLists = lines.par.map { x =>
      val linesp = x.split("\t")
      val msg = linesp.last.replaceAll("[!@#$%^&*()-_+{}[\\]:;\"<,>./?\\]", "")
//      println(msg)
//      val msg = if(linesp.length == 8) linesp(6) else linesp.slice(6, linesp.length-2).mkString(" ")
      val wl = ("\\w+".r findAllIn msg).sliding(3).toSeq

//      println(wl)

      if(wl.size > 0 && wl.last.size == 3 && wl(0).size == 3)
        wl ++ Seq(Seq(wl.last(1), wl.last(2), SENTENCE_END)) ++ Seq(Seq(SENTENCE_START, wl(0)(0), wl(0)(1)))
      else
        wl
    }.flatten.filter(x => x.size == 3).map(x => x.map(y => dictionaryLookup(y)))

    println("Building word map")
    val wordMap = wordLists.groupBy{ x => (x(0), x(1))}.map{ x=> x._1 -> x._2.groupBy(y => y(0)).map(y => y._1 -> y._2.size).toMap}
      .withDefaultValue(Map[Int, Int]())
    file.close()

    wordMap
  }

  def loadAndParseAlexLog(filename: String) = {
    val file = Source.fromFile(filename)
    val lines = file.getLines().toList

    println("Building word list")
    val wordMap = new mutable.HashMap[(Int, Int), mutable.Map[Int, Int]]().withDefaultValue(new mutable.HashMap[Int, Int]().withDefaultValue(0))
    val wordMapByNick = new mutable.HashMap[Int, mutable.Map[(Int, Int), mutable.Map[Int, Int]]]()
      .withDefaultValue(new mutable.HashMap[(Int, Int), mutable.Map[Int, Int]]().withDefaultValue(new mutable.HashMap[Int, Int]().withDefaultValue(0)))
    lines.map { x =>
      val linesp = x.split("\t")
      if(linesp.size > 4) {

        val msg = linesp(4).replaceAll("\\[!@#$%^&*()-_+\\{\\}\\[\\]:;\"<,>./?\\]", "")
        val nick = dictionaryLookup(linesp(1))

        if(!wordMapByNick.contains(nick)) {
          wordMapByNick.put(nick, new mutable.HashMap[(Int, Int), mutable.Map[Int, Int]]()
            .withDefaultValue(new mutable.HashMap[Int, Int]().withDefaultValue(0)))
        }


        val wl = ("\\w+".r findAllIn msg).sliding(3).toSeq.filter(x => x.size == 3)
        val triples = (if (wl.size > 0)
          wl ++ Seq(Seq(wl.last(1), wl.last(2), SENTENCE_END)) ++ Seq(Seq(SENTENCE_START, wl(0)(0), wl(0)(1)))
          else
            wl).map(x => x.map(y => dictionaryLookup(y)))

        triples.foreach { x =>
          //        println(x)
          val tuple = (x(0), x(1))
          if (!wordMap.contains(tuple)) {
            wordMap.put(tuple, new mutable.HashMap[Int, Int]().withDefaultValue(0))
          }
          if (!wordMapByNick(nick).contains(tuple)) {
            wordMapByNick(nick).put(tuple, new mutable.HashMap[Int, Int]().withDefaultValue(0))
          }
          wordMap(tuple).put(x(2), wordMap(tuple)(x(2)) + 1)
          wordMapByNick(nick)(tuple).put(x(2), wordMap(tuple)(x(2)) + 1)


          //        wordMap((x(0), x(1))).put(x(2), wordMap((x(0), x(1)))(x(2)) + 1)
        }
      }

    }

    file.close()

    println("Finished")
    (wordMap, wordMapByNick)
  }

  def loadAndParseAlexLog3(filename: String) = {
    val file = Source.fromFile(filename)
    val lines = file.getLines().toList

    println("Building word list")
    val wordMap3 = new mutable.HashMap[(Int, Int, Int), mutable.Map[Int, Int]]().withDefaultValue(new mutable.HashMap[Int, Int]().withDefaultValue(0))
    lines.map { x =>
      val linesp = x.split("\t")
      if(linesp.size > 4) {
        val msg = linesp(4)

        val wl = ("\\b\\w+\\b".r findAllIn msg).sliding(4).toSeq.filter(x => x.size == 4)
        val triples = (if (wl.size > 0)
          wl ++ Seq(Seq(wl.last(1), wl.last(2), wl.last(3), SENTENCE_END)) ++ Seq(Seq(SENTENCE_START, wl(0)(0), wl(0)(1), wl(0)(2)))
        else
          wl).map(x => x.map(y => dictionaryLookup(y)))

        triples.foreach { x =>
          //        println(x)
          val tuple = (x(0), x(1), x(2))
          if (!wordMap3.contains(tuple)) {
            wordMap3.put(tuple, new mutable.HashMap[Int, Int]().withDefaultValue(0))
          }
          wordMap3(tuple).put(x(3), wordMap3(tuple)(x(3)) + 1)

          //        wordMap((x(0), x(1))).put(x(2), wordMap((x(0), x(1)))(x(2)) + 1)
        }
      }

    }

    file.close()

    wordMap3
  }

  def genSentence(w1: String, w2: String, maxChars:Int = 200): String = {

    var sentence = if(w1 != SENTENCE_START) w1 + " " else ""
    var lastToken = w1
    var selectedToken = w2
    while(selectedToken != SENTENCE_END && sentence.size < maxChars) {
//      lastToken = selectedToken
      if(selectedToken != SENTENCE_START)
        sentence = sentence + selectedToken + " "
      val newTokens = wordMap((dictionaryLookup(lastToken), dictionaryLookup(selectedToken)))
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
            token = revDictionaryLookup(x._1)
        }

//        println(count)

        selectedToken = token
      }

    }

    sentence
  }

  def genNickSentence(nick: String, w1: String, w2: String, maxChars:Int = 200): String = {

    var sentence = if(w1 != SENTENCE_START) w1 + " " else ""
    var lastToken = w1
    var selectedToken = w2
    while(selectedToken != SENTENCE_END && sentence.size < maxChars) {
      //      lastToken = selectedToken
      if(selectedToken != SENTENCE_START)
        sentence = sentence + selectedToken + " "
      val newTokens = wordMapByNick(dictionaryLookup(nick))((dictionaryLookup(lastToken), dictionaryLookup(selectedToken)))
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
            token = revDictionaryLookup(x._1)
        }

        //        println(count)

        selectedToken = token
      }

    }

    sentence
  }

  def genBibleSentence(w1: String, w2: String, maxChars:Int = 200): String = {

    var sentence = if(w1 != SENTENCE_START) w1 + " " else ""
    var lastToken = w1
    var selectedToken = w2
    while(selectedToken != SENTENCE_END && sentence.size < maxChars) {
      //      lastToken = selectedToken
      if(selectedToken != SENTENCE_START)
        sentence = sentence + selectedToken + " "
      val newTokens = wordMapBible((dictionaryLookup(lastToken), dictionaryLookup(selectedToken)))
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
            token = revDictionaryLookup(x._1)
        }

        //        println(count)

        selectedToken = token
      }

    }

    sentence
  }

  //def genSentence3(w1: String, w2: String, w3: String, maxChars:Int = 200): String = {

    //var sentence = if(w1 != SENTENCE_START) w1 + " " else w2 + " "
    //var lastToken = w1
    //var middleToken = w2
    //var selectedToken = w3
    //while(selectedToken != SENTENCE_END && sentence.size < maxChars) {
      ////      lastToken = selectedToken
      //if(selectedToken != SENTENCE_START)
        //sentence = sentence + selectedToken + " "
      //val newTokens = wordMap3((dictionaryLookup(lastToken), dictionaryLookup(middleToken), dictionaryLookup(selectedToken)))
      //if(newTokens.size == 0) {
        //selectedToken = SENTENCE_END
      //} else {
        //lastToken = middleToken
        //middleToken = selectedToken
        //val sum = newTokens.map(x => x._2).sum
        //val idx = random.nextInt(sum) + 1

        ////        println(sum, idx, newTokens)

        //// Now have to map this idx to a word
        //var count = 0
        //var token = ""
        //var lastIdx = 0
        //val t = newTokens.toStream.takeWhile(_ => count < idx).foreach{
          //x =>
            //count += x._2
            ////            if(count < idx)
            //token = revDictionaryLookup(x._1)
        //}

        ////        println(count)

        //selectedToken = token
      //}

    //}

    //sentence
  //}

  def getStartingWords(): (Int, Int) = {
    val startingWords = wordMap.keys.filter(x => x._1 == dictionaryLookup(SENTENCE_START))
    val words = startingWords.toSeq(random.nextInt(startingWords.size))
    words
  }

  def getNickStartingWords(nick: String): (Int, Int) = {
    val startingWords = wordMapByNick(dictionaryLookup(nick)).keys.filter(x => x._1 == dictionaryLookup(SENTENCE_START))
    val words = startingWords.toSeq(random.nextInt(startingWords.size))
    words
  }

  def getStartingWordsBible(): (Int, Int) = {
    val startingWords = wordMapBible.keys.filter(x => x._1 == dictionaryLookup(SENTENCE_START))
    val words = startingWords.toSeq(random.nextInt(startingWords.size))
    words
  }

  //def getStartingWords3(): (Int, Int, Int) = {
    //val startingWords = wordMap3.keys.filter(x => x._1 == dictionaryLookup(SENTENCE_START))
    //val words = startingWords.toSeq(random.nextInt(startingWords.size))
    //words
  //}

  def genRandomSentence(): String = {
    val words = getStartingWords()
    genSentence(revDictionaryLookup(words._1), revDictionaryLookup(words._2))
  }

  def genRandomNickSentence(nick: String): String = {
    if(wordMapByNick.contains(dictionaryLookup(nick))) {
      val words = getNickStartingWords(nick)
      genNickSentence(nick, revDictionaryLookup(words._1), revDictionaryLookup(words._2))
    } else { "" }
  }

  def genRandomNick(): String = {
    revDictionaryLookup(wordMapByNick.keys.toSeq(random.nextInt(wordMapByNick.keys.size)))
  }

  def genRandomSentenceBible(): String = {
    val words = getStartingWordsBible()
    genBibleSentence(revDictionaryLookup(words._1), revDictionaryLookup(words._2))
  }

  //def genRandomSentence3(): String = {
    //val words = getStartingWords3()
    //genSentence3(revDictionaryLookup(words._1), revDictionaryLookup(words._2), revDictionaryLookup(words._3))
  //}
}

object MarkovChainGeneration {
  val mcg = new MarkovChainGeneration("geekboy_dump.2015.03.28")

  def main(args: Array[String]) {

    println(mcg.genSentence(mcg.SENTENCE_START, "isn't"))
    println(mcg.genRandomSentence())
    println(mcg.genRandomSentence())
    println(mcg.genRandomSentence())
    println(mcg.genRandomSentence())

    //println(mcg.genRandomSentence3())
    //println(mcg.genRandomSentence3())
    //println(mcg.genRandomSentence3())
    //println(mcg.genRandomSentence3())

//    val length
  }
}
