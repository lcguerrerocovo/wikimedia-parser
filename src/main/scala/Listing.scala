import java.time.{Instant, ZoneId}
import java.time.format.DateTimeFormatter
import java.util.concurrent.{ThreadLocalRandom, TimeUnit}

/**
  * Created by luisguerrero
  */

case class Listing(id: String, pageTitle: String, createDate: String, action: Option[String], name: Option[String],
                   location_0_coordinate: Option[Double], location_1_coordinate: Option[Double],
                   location: Option[String], content: Option[String], url: Option[String],
                   popularity: Double = 0, freshness: Double = 0, quality: Double = 0)

object Listing {

  def apply(page: String) = new Listing("",page, "2010-01-01", None, None, None, None, None, None, None)

  def apply(page: String, map: Map[String, Option[String]], generate: Boolean): Listing = generate match {
    case true => {
      val listing = apply(page,map)
      val date = Util.toInstant(listing.createDate,Util.zuluPattern)
      val freshness = Math.exp(-((Instant.now().toEpochMilli-date.toEpochMilli))*(5*Math.pow(10,-11)))
      val popularity = ThreadLocalRandom.current().nextInt(1, 101)/100d
      val quality = map.size/25d
      listing.copy(freshness = freshness, popularity = popularity,
        quality = quality)
    }
    case false => apply(page,map)
  }

  def apply(page: String, map: Map[String, Option[String]]): Listing = {
    val date = {
      val edit = map("lastedit").getOrElse("2010-01-01")
      Util.formatInstant(Util.toInstant(if(edit != "") edit else "2010-01-01",Util.wikiPattern))
    }
    val listing = new Listing("", page, date, map("action"), map("name"),
        // latitude
        try {
          Some(map("lat").getOrElse("0").filterNot((x: Char) => x.isWhitespace).toDouble)
        } catch {
          case _: Throwable => None
        },
        // longitude
        try {
          Some(map("long").getOrElse("0").filterNot((x: Char) => x.isWhitespace).toDouble)
        } catch {
          case _: Throwable => None
        },
        // location field format = "lat,long"
        {
          val location = (Util.trim(map("lat").getOrElse("")) + "," +
            Util.trim(map("long").getOrElse("")));
          if (location.startsWith(",") || location.endsWith(","))
            None
          else Some(location)
        },
        map("content"),
        try {
          Some(Util.trim(map("url").get))
        } catch {
          case _: Throwable => None
        })
    listing.copy(id = Util.md5(listing.toString))
  }
}