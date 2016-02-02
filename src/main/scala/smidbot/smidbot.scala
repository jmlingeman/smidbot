package smidbot

import java.io._
import java.net._

import akka.actor.{Actor, ActorSystem, Props}
import akka.routing.RoundRobinPool

import scala.io.Source
import scala.util.Random

case class IRC_Message(message: String)

case class IRC_Response(response: String)

case class NZB_Request(req_string: String, search_terms: String, result_name: String, result_url: String)

case class Concert_Lookup(band: String, zip: String, results: String)

object Smidbot {
  val ircBotNick = "smidbot"
  val ircBotDescription = "smidget's bot"
  val homeChannel = "#geekboy"
  val name: String = "Smidbot"
  println(name + " is alive!")
  val random = new Random()

  val mcg = new MarkovChainGeneration("geekboy_dump.2015.03.28")

  lazy val ps: Stream[Int] = 2 #:: Stream.from(3).filter(i =>
    ps.takeWhile{j => j * j <= i}.forall{ k => i % k > 0})
  var lastPrimeGenerated = Source.fromFile("prime.txt").mkString.stripLineEnd.toInt

  def genNextPrime(): Int = {
    lastPrimeGenerated += 1
    val pw = new PrintWriter("prime.txt")
    pw.write(lastPrimeGenerated.toString)
    pw.close()
    ps(lastPrimeGenerated)
  }

  def main(args: Array[String]) {
    val ircpoll = new IRCPoll(ircBotNick, ircBotDescription, homeChannel)


    // Start up polling actors
    ircpoll.connect("dot", 6667)
    //ircpoll.connect("localhost", 6667)
  }
}

class IRCResponder(connect: Socket, ircBotNick: String, ircBotDescription: String, homeChannel: String) extends Actor {
  val name: String = "IRC Responder Actor"
  println(name + " is alive!")
  val num_parsers = 50
  var mailbox_counter = 0

  val out = new BufferedWriter(new OutputStreamWriter(connect.getOutputStream()))

  val ircParsers = context.actorOf(RoundRobinPool(5).props(Props[IRCParser]), "IRCParsers")

  setup()

  def receive = {
    // This thread will throttle the parsing threads by only allowing one thing to write at once.
    // So when data is sent back to here, write it out in the order that it came in.
    case message: IRC_Message =>
      // Figure out which parser to send this message to and send it
      println("SENDING TO PARSER")
      ircParsers ! message

    case response: IRC_Response =>
      // Send out the response contained here
      println("GOT RESPONSE FROM PARSER")
      sendData(response.response)
  }

  def sendData(ircDataOutput: String) {
    out.write(ircDataOutput);
    out.newLine();
    out.flush();
  }

  // Set up our parser actor pool.
  def setup() {
    sendData("NICK " + ircBotNick);
    sendData("USER " + ircBotNick + " 8 * " + ircBotDescription);
    sendData("JOIN " + homeChannel);

    sendData("Beep boop boop beep.");
  }
}

class IRCPoll(ircBotNick: String, ircBotDescription: String, homeChannel: String) {

  def connect(connectingAddress: String, connectingPort: Int) {
    val connect = new Socket(connectingAddress, connectingPort);
    //val out = new BufferedWriter(new OutputStreamWriter(connect.getOutputStream()));
    val in = new BufferedReader(new InputStreamReader(connect.getInputStream))
    setUp(connect, in)
  }

  def setUp(connect: Socket, in: BufferedReader) {
    // Make our response actor
    val system = ActorSystem.create("IRCSystem")
    val responder = system.actorOf(Props(classOf[IRCResponder], connect, ircBotNick, ircBotDescription, homeChannel), name = "responder")


    //    val responder = new IRCResponder(connect, ircBotNick, ircBotDescription, homeChannel)
    //    responder.start()

    while (true) {
      val line = in.readLine();

      if (line != null) {
        if (line.substring(0, 4).equalsIgnoreCase("ping")) {
          val pongmsg = "pong " + line.substring(5);
          responder ! IRC_Response(pongmsg)
        }
        else {
          responder ! IRC_Message(line)
          println("SENT TO RESPONDER")
          println(line)
        }
      }
    }
  }
}

class IRCParser() extends Actor {
  val name: String = "IRC Parser"
  println(name + " is alive!")

  // var file_name: String = new String
  // var file_buffer

  // When a message comes in, figure out what to do with it, do that, then
  // send it back to the polling thread.
  def toChat(txt: String, channel: String): String = {
    if (txt == "")
      return "PRIVMSG " + channel + " :Sorry, I can't seem to find that."
    if (txt.contains("firefly"))
      return "PRIVMSG " + channel + " :ARGLE BLARGLE"
    val chat = "PRIVMSG " + channel + " :" + txt
    return deHTMLify(chat)
  }

  def deHTMLify(txt: String): String = {
    return txt.replace("&quot;", "\"")
  }

  def receive = {
    case message: IRC_Message =>
      val channel = Smidbot.homeChannel
      // if ( (message.message contains "#geekboy") )
      //    channel = "#geekboy"
      //else
//        channel = message.message.substring(1, message.message.indexOf("!")).trim
      val line = message.message
      println("GOT MESSAGE!!!")
      println("MSG: ", line)
      println("CHANNEL is currently set to " + channel)
      //if (( line contains "timmaha") || (line contains "timanus") || (line contains "timshark")) {

      //}
      if (line contains "smidbot test") {
        println("Testing smidbot")
        sender ! IRC_Response(toChat("SMIDBOT IS BEING TESTED EEEEE", channel))
      }
      else if (line.contains(Smidbot.ircBotNick + ":") || line.contains(Smidbot.ircBotNick + ",")) {
        val msg = line.split(":").toSeq.last
        val msgSp = msg.split(" ")

        val idx1 = Smidbot.random.nextInt(msgSp.size-1)
        val idx2 = idx1+1

        sender ! IRC_Response(toChat(Smidbot.mcg.genSentence(msgSp(idx1), msgSp(idx2)), channel))
      }
      //else if (line.contains("!gensentence3") || line.contains("!gs3")) {
        //println("Sending sentence")
        //val linesp = line.split(" ")
        //val startIdx = linesp.indexWhere(x => x.contains("!gensentence3") || x.contains("!gs3"))
        //if(linesp.size > startIdx + 3) {
          //val word1 = linesp(startIdx + 1)
          //val word2 = linesp(startIdx + 2)
          //val word3 = linesp(startIdx + 3)

          //sender ! IRC_Response(toChat(Smidbot.mcg.genSentence3(word1, word2, word3), channel))
        //} else if (linesp.size > startIdx + 2) {
          //val word1 = Smidbot.mcg.SENTENCE_START
          //val word2 = linesp(startIdx + 1)
          //val word3 = linesp(startIdx + 2)

          //sender ! IRC_Response(toChat(Smidbot.mcg.genSentence3(word1, word2, word3), channel))
        //} else {
          //sender ! IRC_Response(toChat(Smidbot.mcg.genRandomSentence3(), channel))
        //}
      //}
      else if (line.contains("!gensentence") || line.contains("!gs")) {
        println("Sending sentence")
        val linesp = line.split(" ")
        val startIdx = linesp.indexWhere(x => x.contains("!gensentence") || x.contains("!gs"))
        if(linesp.size > startIdx + 2) {
          val word1 = linesp(startIdx + 1)
          val word2 = linesp(startIdx + 2)

          sender ! IRC_Response(toChat(Smidbot.mcg.genSentence(word1, word2), channel))
        } else if (linesp.size > startIdx + 1) {
          val word1 = Smidbot.mcg.SENTENCE_START
          val word2 = linesp(startIdx + 1)

          sender ! IRC_Response(toChat(Smidbot.mcg.genSentence(word1, word2), channel))
        } else {
          sender ! IRC_Response(toChat(Smidbot.mcg.genRandomSentence(), channel))
        }
      }
      else if (line.contains("!gennicksentence") || line.contains("!gns")) {
        println("Sending sentence")
        val linesp = line.split(" ")
        val startIdx = linesp.indexWhere(x => x.contains("!gennicksentence") || x.contains("!gns"))
        if(linesp.size > startIdx + 3) {
          val nick = linesp(startIdx + 1)
          val word1 = linesp(startIdx + 2)
          val word2 = linesp(startIdx + 3)

          sender ! IRC_Response(toChat(s"<$nick> " + Smidbot.mcg.genNickSentence(nick, word1, word2), channel))
        } else if (linesp.size > startIdx + 2) {
          val nick = linesp(startIdx + 1)
          val word1 = Smidbot.mcg.SENTENCE_START
          val word2 = linesp(startIdx + 2)

          sender ! IRC_Response(toChat(s"<$nick> " + Smidbot.mcg.genNickSentence(nick, word1, word2), channel))
        }
          else if (linesp.size > startIdx + 1) {
            val nick = linesp(startIdx + 1)
            sender ! IRC_Response(toChat(s"<$nick> " + Smidbot.mcg.genRandomNickSentence(nick), channel))

        } else {
          val nick = Smidbot.mcg.genRandomNick()
          sender ! IRC_Response(toChat(s"<$nick> " + Smidbot.mcg.genRandomNickSentence(nick), channel))
        }
      }
      else if (line.contains("!genbiblesentence") || line.contains("!gbs")) {
        println("Sending sentence")
        val linesp = line.split(" ")
        val startIdx = linesp.indexWhere(x => x.contains("!genbiblesentence") || x.contains("!gbs"))
        if(linesp.size > startIdx + 2) {
          val word1 = linesp(startIdx + 1)
          val word2 = linesp(startIdx + 2)

          sender ! IRC_Response(toChat(Smidbot.mcg.genBibleSentence(word1, word2), channel))
        } else if (linesp.size > startIdx + 1) {
          val word1 = Smidbot.mcg.SENTENCE_START
          val word2 = linesp(startIdx + 1)

          sender ! IRC_Response(toChat(Smidbot.mcg.genBibleSentence(word1, word2), channel))
        } else {
          sender ! IRC_Response(toChat(Smidbot.mcg.genRandomSentenceBible(), channel))
        }
      }
      else if (line contains "primetime") {
        sender ! IRC_Response(toChat(Smidbot.genNextPrime().toString, channel))
      }
      else {
      }

  }


  def getWebpage(addr: String): String = {
    try {
      println("Downloading webpage " + addr)
      val url: URL = new URL(addr)
      val is: InputStream = url.openStream()
      val dis: DataInputStream = new DataInputStream(new BufferedInputStream(is))
      var line: String = ""
      var page: String = ""

      line = dis.readLine()
      while (line != null) {
        page = page + line + "\n"
        line = dis.readLine()
      }
      dis.close()
      is.close()
      println(page)
      return page
    }
    catch {
      case e: Exception =>
        println("EXCEPTION")
        return ""
    }
  }

  def stripTags(line: String): String = {
    return line.replaceAll("<.*?>", "").trim
  }
}


