import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by luisguerrero
  */
class WikiMediaParserSpec  extends FlatSpec with Matchers with WikiMediaListing {

  it should "parse a simple wikimedia listing properly" in {
      val txt = """see| name=Split Point Lighthouse | content =  Affectionately known as The White Queen"""

      parseAll(listing("whatever"),txt).get.content should equal (Some("Affectionately known as The White Queen"))
  }
}
