import java.io._

import io.circe.generic.auto._
import io.circe.syntax._

import scala.collection.LinearSeq
import scala.collection.immutable.Nil
import scala.io.Source

/**
  * Created by luisguerrero
  */

object WikiMediaParser extends App with WikiMediaListing with TextParser {

  var listingsSoFar = 0
  var pagesRead = 0

  val usage =
    """ Usage: sbt \"run filenameToRead filenameTowrite""
      |    ex: sbt \"run enwikivoyage-20170101-pages-articles.xml listings.json\" """.stripMargin

  if (args.length < 2) {println(usage); System.exit(1)}

  val stream = new FileInputStream(args(0))
  val src = Source.fromInputStream(stream, "UTF-8")

  try {
    parsePage(src.getLines().toStream, Nil)
  } catch {
    case e: java.lang.Exception => println("script finished processing wiki file," +
      " processed [" + pagesRead + "] locations and [" + listingsSoFar + "] listings")
  }


  def parsePage(xml: LinearSeq[String], listings: List[Listing]): List[Listing] = {

    def trim(line: String, start: String, end: String): String
    = (line.replaceAll(s"^.*?$start", "").split(s"$end.*?($start|$$)")) mkString ""

    def isRedirect(page: Vector[String]): Boolean = (page mkString "").indexOf("<redirect ") != -1

    def captureTag(start: String, end: String, xml: LinearSeq[String]): (LinearSeq[String], LinearSeq[String]) = {
      val segment = xml.dropWhile(_.indexOf(start) == -1)
      val page = segment.takeWhile(_.indexOf(end) == -1)
      (page, segment.dropWhile(_.indexOf(end) == -1))
    }

    if (!xml.isEmpty) {
      val (page, next) = captureTag("<page>", "</page>", xml)
      val pageList = page.toVector :+ next.headOption.getOrElse(
        throw new Exception("no more pages to read from wiki database"))
      if (isRedirect(pageList)) parsePage(next.tail, listings)
      else {
        pagesRead += 1
        println("read " + (pagesRead) + " pages so far with listings")

        val (ti, tn) = captureTag("<title>", "</title>", pageList.toList)
        val (tx, xn) = captureTag("<text xml:space=\"preserve\">", "</text>", pageList.toList)
        val title = trim((ti.toVector :+ tn.headOption.getOrElse(Nil)) mkString "\n", "<title>", "</title>")
        val text = trim((tx.toVector :+ xn.headOption.getOrElse(Nil)) mkString "\n", "<text xml:space=\"preserve\">", "</text>")

        println("going to process [" + title + "] location")
        val lst = parseText(text)
        val lst2 = lst.map(buildListing(_, title)).flatten//::: listings
        listingsSoFar += lst2.size
        
        println("processed " + (listingsSoFar) + " listings so far")
        writeListingsToFile(lst2)
        parsePage(next.tail, listings)
      }
    } else listings
  }


  def writeListingsToFile(lst: List[Listing]): Unit = {
    try {
      val writer = new FileWriter("data/listings-" + pagesRead + ".json")
      val pw = new PrintWriter(writer)
      pw.write(lst.asJson.toString)
      pw.close()
    } catch {
      case e: IOException => {
        println("error writing listings ")
        lst.foreach(println)
        println("please review listings output ")
      }
    }
  }

  def buildListing(text: String, pageTitle: String): Option[Listing] = {
    if(List("see","do") exists (text.stripPrefix("{{").trim startsWith _))  {
      try {
        Some(parseAll(listing(pageTitle), text).get)
      } catch {
        case e: Exception => println("problem processing listing because of " +e); println(text); None
      }
    }
    else None
  }

}