import java.io._

import io.circe.generic.auto._
import io.circe.syntax._

import scala.collection.LinearSeq
import scala.collection.immutable.Nil
import scala.io.Source
import scala.util.parsing.combinator._


/**
  * Created by luisguerrero
  */

case class Listing(pageTitle: String, action: Option[String], name: Option[String], lat: Option[String],
                   long: Option[String], content: Option[String])

object Listing {
  def apply(page: String) = new Listing(page, None, None, None, None, None)

  def apply(page: String, map: Map[String, Option[String]]) = new Listing(page, map("action"), map("name"),
    map("lat"), map("long"), map("content"))
}


trait WikiMediaListing extends RegexParsers {
  override def skipWhitespace = true

  def listing(page: String): Parser[Listing] = name ~ "|" ~ repsep(pair, "|") ^^ {
    {
      case name ~ "|" ~ mp => Listing(page, (Map("action" -> Some(name)).withDefaultValue(None) ++ mp))
    }
  }

  def pair: Parser[(String, Option[String])] = attr ~ "=" ~ text.? ^^ { x => (x._1._1, x._2) }

  def name: Parser[String] = ("see" | "do")

  def attr: Parser[String] = ("checkin" | "checkout" | "name" | "alt" | "email" | "url" | "address" | "lat" | "long" | "wikidata" |
    "directions" | "phone" | "tollfree" | "fax" | "hours" | "price" | "lastedit" | "content" | "wikipedia" | "image")

  def text: Parser[String] = """[^|\n\r]*""".r | ""
}


object WikiMediaParser extends App with WikiMediaListing {


  // these are locations which had issues and no listings could be extracted from them
  val locationsWithIssues = List("Angoulême", "Droitwich", "Gödöllő", "Lehigh Valley", "Louisville", "Munich",
    "Newark (Ohio)", "Pretoria", "Rosemount", "Samarkand", "Sauk Centre", "Savannah", "Spring Grove (Minnesota)",
    "Stoughton (Wisconsin)", "Varaždin", "Vega (Texas)", "Template:Easter Day", "Template:Easter Monday",
    "Template:Good Friday", "Template:Pentecost", "Template:Whit Monday", "Nelson (England)", "Mabinay",
    "Edipsos")

  var listingsSoFar = 0
  var pagesRead = 0

  val usage =
    """ Usage: sbt \"run filenameToRead filenameTowrite""
      |    ex: sbt \"run enwikivoyage-20170101-pages-articles.xml listings.json\" """.stripMargin

  if (args.length < 2) {println(usage); System.exit(1)}

  val stream = new FileInputStream(args(0))
  val src = Source.fromInputStream(stream, "UTF-8")
  val writer = new FileWriter(/*args(1)*/ "listings.json", true)
  val bfwriter = new BufferedWriter(writer)
  val pw = new PrintWriter(bfwriter)

  try {
    parsePage(src.getLines().toStream, Nil)
  } catch {
    case e: java.lang.Exception => println("script finished processing wiki file")
  }
  pw.close()


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

        if (!locationsWithIssues.contains(title)) {
          val lst = parseText(text)
          val lst2 = lst.map(buildListing(_, title)) //::: listings

          listingsSoFar += lst2.size
          println("processed " + (listingsSoFar) + " listings so far")
          writeListingsToFile(lst2)
        }
        parsePage(next.tail, listings)
      }
    } else listings
  }


  def writeListingsToFile(lst: List[Listing]): Unit = {
    try {
      pw.write(lst.asJson.toString)
    } catch {
      case e: IOException => {
        println("error writing listings ")
        lst.foreach(println)
        println("please review listings output ")
      }
    }
  }


  def parseText(text: String): List[String] = {
    //val all = "\\{\\{(see|do)[^\\}]*\\}\\}".r.findAllIn(text)
    val all = "\\{\\{(see|do)([^{{}}]*|\\{\\{[^{{}}]*\\}\\})*\\}\\}".r.findAllIn(text)
    val bremoved = all.map(x => (x.toCharArray.filter(_ >= ' ')).drop(2).dropRight(2) mkString "")
    bremoved.map(_.replaceAll("\\{\\{[^{{}}]*\\}\\}", "")).toList
  }

  def buildListing(text: String, pageTitle: String): Listing = {
    parseAll(listing(pageTitle), text)
      .getOrElse({
        println("there was a problem with the following listing [" + text + "]"); Listing(pageTitle)
      })
  }

}