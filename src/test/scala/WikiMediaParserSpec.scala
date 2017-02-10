import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by luisguerrero
  */
class WikiMediaParserSpec  extends FlatSpec with Matchers with WikiMediaListing {

  it should "parse a simple wikimedia listing properly" in {
      val txt = """see| name=Split Point Lighthouse | content =  Affectionately known as The White Queen"""

      parseAll(listing("whatever"),txt).get should equal (Listing("whatever",Some("see"),
        Some("Split Point Lighthouse "),
      None,None,Some("Affectionately known as The White Queen")))
  }

  it should "parse a more complex wikimedia listing properly" in {
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
                       | workshop, and a museum.""".stripMargin)))
  }
}
