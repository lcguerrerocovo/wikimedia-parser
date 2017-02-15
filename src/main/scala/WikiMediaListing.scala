import scala.util.matching.Regex
import scala.util.parsing.combinator.{RegexParsers, SubSequence}
import scala.util.parsing.combinator
import scala.util.parsing.combinator.syntactical.StandardTokenParsers

/**
  * Created by luisguerrero
  */
trait WikiMediaListing extends RegexParsers {
  override def skipWhitespace = true

  def listing(page: String): Parser[Listing] = "{{" ~ name ~ "|" ~ repsep(pair2, "|") ~ "}}" ^^ {
    {
      case "{{" ~ name ~ "|" ~ mp ~ "}}" => Listing(page,
        (Map("action" -> Some(name)).withDefaultValue(None) ++ mp.map(x => (x._1,x._2))))
    }
  }

  def pair: Parser[(String, Option[String])] = attr ~ "=" ~
    opt(text) ^^ {
    case x ~ "=" ~ y => (x,y)
  }

  def pair2: Parser[(String, Option[String])] = attr ~ "=" ~
    opt(text) ~ opt(innerPair) ~ opt(text) ^^ {
      case x ~ "=" ~ y1 ~ z ~ y2 => (x, Some(y1.getOrElse("") + z.getOrElse(("",""))._1 + y2.getOrElse("")))
  }

  def innerPair: Parser[(String, String)] = ("{{"|"[[") ~ text ~ "|" ~ text ~ ("}}"|"]]") ^^ {
    {
      case (("{{"|"[[") ~ x ~ "|" ~ y ~ ("}}"|"]]")) => (x, y)
    }
  }
  
  def name: Parser[String] = ("see" | "do")

  def attr: Parser[String] = ("checkin" | "checkout" | "name" | "alt" | "email" | "url" | "address" | "lat" | "long" | "wikidata" |
    "directions" | "phone" | "tollfree" | "fax" | "hours" | "price" | "lastedit" | "content" | "wikipedia" | "image")

  def text: Parser[String] = """[^\||\{\{|\}\}|\[\[|\]\]]+?(?=(\r|\n|\||\{\{|\}\}|\[\[|\]\]|$))""".r | ""

}