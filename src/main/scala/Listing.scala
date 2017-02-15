/**
  * Created by luisguerrero on 2/14/17.
  */
case class Listing(id: String, pageTitle: String, action: Option[String], name: Option[String], location_0_coordinate: Option[Double],
                   location_1_coordinate: Option[Double], location: Option[String], content: Option[String])

object Listing {
  def apply(page: String) = new Listing(java.util.UUID.randomUUID.toString,page, None, None, None, None, None, None)

  def apply(page: String, map: Map[String, Option[String]]) = new Listing(java.util.UUID.randomUUID.toString,page,
    map("action"), map("name"), try { Some(map("lat").getOrElse("0").filterNot((x: Char) => x.isWhitespace).toDouble) }
    catch { case _ => None }, try { Some(map("long").getOrElse("0").filterNot((x: Char) => x.isWhitespace).toDouble) }
    catch { case _ => None }, {val location = (map("lat").getOrElse("").filterNot((x: Char) => x.isWhitespace) + "," +
      map("long").getOrElse("").filterNot((x: Char) => x.isWhitespace)); if(location.startsWith(",") || location.endsWith(","))
      None else Some(location)}, map("content"))
}