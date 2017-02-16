import java.text.SimpleDateFormat
import java.time.{Instant, ZoneId}
import java.time.format.DateTimeFormatter

/**
  * Created by luisguerrero on 2/15/17.
  */
object Util {

  val zuluPattern = "yyyy-MM-dd'T'HH:mm:ss.SSS'Z'"
  val wikiPattern = "yyyy-MM-dd"
  
  val zuluFormatter = DateTimeFormatter.ofPattern(zuluPattern)
  val wikiFormatter = DateTimeFormatter.ofPattern(wikiPattern)

  def toInstant(date: String, pattern: String)
    = new SimpleDateFormat(pattern).parse(date).toInstant

  def formatInstant(ins: Instant): String
    = ins.atZone(ZoneId.of("America/Sao_Paulo")).toLocalDateTime.format(zuluFormatter)

  def generateID = java.util.UUID.randomUUID.toString

  def trim(str: String) = str.filterNot((x: Char) => x.isWhitespace)

}
