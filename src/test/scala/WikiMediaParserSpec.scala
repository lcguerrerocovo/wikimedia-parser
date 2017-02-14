import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by luisguerrero
  */
class WikiMediaParserSpec  extends FlatSpec with Matchers with WikiMediaListing {

  it should "parse a simple wikimedia listing properly" in {
      val txt = """see| name=Split Point Lighthouse | content =  Affectionately known as The White Queen"""

      val lst: Listing = parseAll(listing("whatever"),txt).getOrElse(Listing("whatever"))
    
    
    lst.copy(id="") should equal (Listing("whatever", Map("action" -> Some("see"),
      "name" -> Some("Split Point Lighthouse "), "lat" -> None,
      "long" -> None, "location" -> None,
      "content" -> Some("Affectionately known as The White Queen"))).copy(id=""))
  }

  it should "parse a simple wikimedia listing with url and dead link properly" in {
    val txt =
      """see| name=Split Point Lighthouse | url=http://www.isbryderen-elbjorn.dk {{dead link|May 2016}} |
        |content =  Affectionately known as The White Queen""".stripMargin

    val lst: Listing = parseAll(listing("whatever"),txt).getOrElse(Listing("whatever"))


    lst.copy(id="") should equal (Listing("whatever", Map("action" -> Some("see"),
      "name" -> Some("Split Point Lighthouse "), "lat" -> None,
      "long" -> None, "location" -> None,
      "content" -> Some("Affectionately known as The White Queen"))).copy(id=""))
  }


  it should "parse an url pair" in {
    val txt = """url=http://www.isbryderen-elbjorn.dk {{dead link|May 2016}} """

    val pr = parseAll(pair, txt).getOrElse(None)


    pr should equal (("url",Some("http://www.isbryderen-elbjorn.dk "),Some(("dead link","May 2016"))))
  }
}

  //Listing(id: String, pageTitle: String, action: Option[String], name: Option[String], location_0_coordinate: Option[Double],
  //  location_1_coordinate: Option[Double], location: Option[String], content: Option[String])

  /*it should "parse a more complex wikimedia listing properly" in {
    val txt =
      """see| name = Saint John's Cathedral | alt=Sint Jans Kathedraal | url = | email = | address = | lat = 51.68808 |
        | long=5.30814 | directions= | phone= | tollfree = | fax = | hours = | price=| lastedit=2016-01-25 |
        | content=one of the most prominent landmarks of Den Bosch. Building started in 1380, in Gothic style.
        | The exterior of the building had been deteriorating fast due to acid rain and restoration works started
        | in 1960. It has taken many years to restore the full church, but the works are completed and the church
        | can be seen in all its glory. The restoration also included the interior. Of course some minor maintainance
        | takes place constantly.""".stripMargin

    parseAll(listing("whatever"),txt).get should equal (Listing("whatever",Some("see"),Some("Saint John's Cathedral"),
      Some("51.68808"),Some("51.68808"),Some("""one of the most prominent landmarks of Den Bosch. Building started in 1380, in Gothic style.
                                                |The exterior of the building had been deteriorating fast due to acid rain and restoration works started
                                                |in 1960. It has taken many years to restore the full church, but the works are completed and the church
                                                |can be seen in all its glory. The restoration also included the interior. Of course some minor maintainance
                                                |takes place constantly.""".stripMargin)))
  }

  it should "parse a wikimedia listing with urls properly" in {
    val txt ="""do| name=Elbjørn | url=http://www.isbryderen-elbjorn.dk | email=info@isbryderen-elbjorn.dk|
        | address= | lat= | long= | directions=| phone= | tollfree= | fax=| hours= | price=| content=Icebreaker
        | now working as a restaurant and culture ship at the Aalborg harbour. It has a restaurant, a bar, glass
        | workshop, and a museum.""".stripMargin

    parseAll(listing("whatever"),txt).get should equal (Listing("whatever",Some("see"),Some("Elbjørn"),
      None,None,Some("""Icebreaker now working as a restaurant and culture ship at the Aalborg harbour. It has a restaurant, a bar, glass
                       | workshop, and a museum.""".stripMargin))) */
  

