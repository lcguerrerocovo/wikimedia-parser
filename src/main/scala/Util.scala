import java.time.{Instant, ZoneId}
import java.time.format.DateTimeFormatter

/**
  * Created by luisguerrero on 2/15/17.
  */
object Util {

  val zuluPattern = "yyyy-MM-dd'T'HH:mm:ss.SSS'Z'"
  val fmt = DateTimeFormatter.ofPattern(zuluPattern)

  def formatInstant(ins: Instant): String = ins.atZone(ZoneId.of("America/Sao_Paulo")).toLocalDateTime.format(fmt)

}
