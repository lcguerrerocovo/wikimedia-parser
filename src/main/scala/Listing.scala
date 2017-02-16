import java.time.{Instant, ZoneId}
import java.time.format.DateTimeFormatter
import java.util.concurrent.{ThreadLocalRandom, TimeUnit}

/**
  * Created by luisguerrero on 2/14/17.
  */

case class Listing(id: String, pageTitle: String, action: Option[String], name: Option[String],
                   location_0_coordinate: Option[Double], location_1_coordinate: Option[Double],
                   location: Option[String], content: Option[String], popularity: Double = 0,
                   freshness: Double = 0, quality: Double = 0, createDate: String = Util.formatInstant(Instant.now))

object Listing {
  def apply(page: String) = new Listing(java.util.UUID.randomUUID.toString,page, None, None, None, None, None, None)

  def apply(page: String, map: Map[String, Option[String]], fake: Boolean): Listing = fake match {
    case true => {
      val listing = apply(page,map)
      val date: Long = Instant.now().toEpochMilli -
        (TimeUnit.DAYS.toMillis(ThreadLocalRandom.current().nextInt(0, 90)))
      val freshness = Math.exp(-((Instant.now().toEpochMilli-date))*0.0000000005)
      val popularity = ThreadLocalRandom.current().nextInt(1, 101)/100d
      val quality = map.size/25d
      listing.copy(createDate = Util.formatInstant(Instant.ofEpochMilli(date)), freshness = freshness,
        popularity = popularity, quality = quality)
    }
    case false => apply(page,map)
  }

  def apply(page: String, map: Map[String, Option[String]]) = new Listing(java.util.UUID.randomUUID.toString,
    page, map("action"), map("name"),
    // latitude
    try { Some(map("lat").getOrElse("0").filterNot((x: Char) => x.isWhitespace).toDouble)
    } catch { case _ => None },
    // longitude
    try { Some(map("long").getOrElse("0").filterNot((x: Char) => x.isWhitespace).toDouble)
    } catch { case _ => None },
    // location field format = "lat,long"
    {val location = (map("lat").getOrElse("").filterNot((x: Char) => x.isWhitespace) + "," +
      map("long").getOrElse("").filterNot((x: Char) => x.isWhitespace));
      if(location.startsWith(",") || location.endsWith(","))
      None else Some(location)},
    map("content"))
}