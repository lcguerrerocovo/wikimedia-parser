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

  // no longer used, just kept here until we establish we can parse all listings this way
  def pair: Parser[(String, Option[String])] = attr ~ "=" ~
    opt(text) ^^ {
    case x ~ "=" ~ y => (x,y)
  }

  def pair2: Parser[(String, Option[String])] = attr ~ "=" ~
    opt(text) ~ rep(innerPair ~ text) ^^ {
      case x ~ "=" ~ y1 ~ z => (x,
        Some(y1.getOrElse("") + z.map(x => x._1 + x._2).foldLeft("")(_ + _)))
  }

  def innerPair: Parser[String] = ("{{"|"[[") ~ text ~ opt("|") ~ opt(text) ~ ("}}"|"]]") ^^ {
    {
      case (("{{"|"[[") ~ x ~ Some("|") ~ y ~ ("}}"|"]]")) => x
      case (("{{"|"[[") ~ x ~ None ~ Some("") ~ ("}}"|"]]")) => x + " "
      //case ("[" ~ x ~ Some("|") ~ Some(y) ~ "]") => x + " "
      //case ("[" ~ x ~ None ~ Some("") ~ "]") => x + " "
    }
  }
  
  def name: Parser[String] = ("see" | "do")

  def attr: Parser[String] = ("checkin" | "checkout" | "name" | "alt" | "email" | "url" | "address" | "lat" | "long" | "wikidata" |
    "directions" | "phone" | "tollfree" | "fax" | "hours" | "price" | "lastedit" | "content" | "wikipedia" | "image")

  def text: Parser[String] = """(^|\r|\n|\||\{\{|\}\}|\[\[|\]\]).*?(?=(\r|\n|\||\{\{|\}\}|\[\[|\]\]|$))""".r | ""

}