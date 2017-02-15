import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by luisguerrero
  */
class WikiMediaParserSpec  extends FlatSpec with Matchers with WikiMediaListing {

  it should "parse a simple wikimedia listing properly" in {
    val txt = """{{see| name=Split Point Lighthouse | content =  Affectionately known as The White Queen}}"""

    val lst: Listing = parseAll(listing("whatever"),txt).getOrElse(Listing("whatever"))

    lst.copy(id="") should equal (Listing("whatever", Map("action" -> Some("see"),
      "name" -> Some("Split Point Lighthouse "), "lat" -> None,
      "long" -> None, "location" -> None,
      "content" -> Some("Affectionately known as The White Queen"))).copy(id=""))
  }

  it should "parse a simple wikimedia listing with url and dead link properly" in {
    val txt = """{{see| name=Split Point Lighthouse | url=http://www.isbryderen-elbjorn.dk {{dead link|May 2016}} |
        |content =  Affectionately known as The White Queen}}""".stripMargin

    val lst: Listing = parseAll(listing("whatever"),txt).getOrElse(Listing("whatever"))

    lst.copy(id="") should equal (Listing("whatever", Map("action" -> Some("see"),
      "name" -> Some("Split Point Lighthouse "), "lat" -> None,
      "long" -> None, "location" -> None,
      "content" -> Some("Affectionately known as The White Queen"))).copy(id=""))
  }


  it should "parse an url pair" in {
    val txt = """url=http://www.isbryderen-elbjorn.dk {{dead link|May 2016}} """

    val pr = parseAll(pair2, txt).getOrElse(None)

    pr should equal (("url",Some("http://www.isbryderen-elbjorn.dk dead link")))
  }

  it should "parse content with inner" in {
    val txt =
      """content=There are many temples in and around Almora.One can visit
        | '''Chitai Temple''', 8 km from city, and a group of temples in
        | [[Jageshwar | Jageshwar]] (20 km from Almora).""".stripMargin.replaceAll("\n", "")

    val pr = parseAll(pair2, txt).get
    
    pr should equal (("content", Some(
      """There are many temples in and around Almora.One can visit
        | '''Chitai Temple''', 8 km from city, and a group of temples in Jageshwar
        | (20 km from Almora).""".stripMargin.replaceAll("\n", ""))))
  }

  it should "parse content with multiples 'inners'" in {
    val txt ="""content=Built 1864-1867 as the new residence of the resident of the residency of [[Parahyangan]], as its capital was moved
               | from [[Cianjur]] to Bandung. The Indies Empire style building is nowadays used as the
               | official residence of the governor of West Java province.""".stripMargin.replaceAll("\n", "")

    val pr = parseAll(pair2, txt).get

    // indexing adds a space because the parser is eating leading whitespace after it processes these inners

    pr should equal (("content", Some("""Built 1864-1867 as the new
             | residence of the resident of the residency of Parahyangan , as its capital was moved
             | from Cianjur to Bandung. The Indies Empire style building is nowadays used as the
             | official residence of the governor of West Java province.""".stripMargin.replaceAll("\n", ""))))
  }



  it should "parse realistic listing" in {
    val txt = """{{see
                || name=Saint John's Cathedral | alt=Sint Jans Kathedraal | url= | email=
                || address= | lat=51.68808 | long=5.30814 | directions=
                || phone= | tollfree= | fax=
                || hours= | price=
                || lastedit=2016-01-25
                || content=one of the most prominent landmarks of Den Bosch. Building started in 1380,
                | in Gothic style. The exterior of the building had been deteriorating fast due to acid
                | rain and restoration works started in 1960. It has taken many years to restore the
                | full church, but the works are completed and the church can be seen in all its glory.
                | The restoration also included the interior. Of course some minor maintainance takes
                | place constantly.
                |}}""".stripMargin.replaceAll("\n", "")

    val lst = parseAll(listing("whatever"), txt).getOrElse(Listing("whatever"))

    lst.copy(id="") should equal (Listing("","whatever",Some("see"),Some("Saint John's Cathedral "),
      Some(51.68808),Some(5.30814),Some("51.68808,5.30814"),Some(
        """one of the most prominent landmarks of Den Bosch. Building started in 1380,
          | in Gothic style. The exterior of the building had been deteriorating fast
          | due to acid rain and restoration works started in 1960. It has taken many
          | years to restore the full church, but the works are completed and the
          | church can be seen in all its glory. The restoration also included the
          | interior. Of course some minor maintainance takes place constantly.""".stripMargin.replaceAll("\n", ""))))
  }

  it should "parse a second realistic listing" in {
    val txt = """{{see
                || name=Alnwick Castle | alt= | url=http://www.alnwickcastle.com/ | email=
                || address= | lat=55.41575 | long=-1.70607 | directions=
                || phone= | tollfree= | fax=
                || hours= | price=
                || wikipedia=Alnwick Castle
                || content=The &quot;poison garden&quot; tour is entertaining. Seeing the
                | castle might be a thrill for people who are really big fans of Rowan
                | Atkinson's &quot;Black Adder&quot; or the Harry Potter films.
                |}} """.stripMargin.replaceAll("\n", "")

    val lst = parseAll(listing("whatever"), txt).getOrElse(Listing("whatever"))

    lst.copy(id="") should equal (Listing("","whatever",Some("see"),Some("Alnwick Castle "),
      Some(55.41575),Some(-1.70607),Some("55.41575,-1.70607"),Some(
        """The &quot;poison garden&quot; tour is entertaining. Seeing the castle might be a thrill
          | for people who are really big fans of Rowan Atkinson's &quot;Black Adder&quot; or the
          | Harry Potter films.""".stripMargin.replaceAll("\n", ""))))
  }

  it should "parse a third realistic listing" in {
    val txt =
      """{{see| name=Temples Nearby | url= | email=| address= | lat= | long= |
        | directions=| phone= | tollfree= | fax=| hours= | price=| content=There
        | are many temples in and around Almora. One can visit '''Chitai Temple''',
        | 8 km from city, and a group of temples in [[Jageshwar|Jageshwar]]
        | (20 km from Almora).}}""".stripMargin.replaceAll("\n", "")

    val lst = parseAll(listing("whatever"), txt).getOrElse(Listing("whatever"))

    lst.copy(id="") should equal (Listing("","whatever",Some("see"),Some("Temples Nearby "),
      None,None,None,
      Some("""There are many temples in and around Almora. One can visit '''Chitai Temple''',
        | 8 km from city, and a group of temples in Jageshwar(20 km from Almora)."""
        .stripMargin.replaceAll("\n", ""))))
  }

  it should "parse listing when name has inner 'pair' with only one member" in  {
    val txt =
      """{{do| name= ''Prašivá'' or ''Veľká Chocuľa'' peaks in [[Low Tatras]] |
        | alt= | url= | email=| address= | lat=48.8938 | long=19.3310 |
        | directions=Start in ''Liptovská Lúžna'' or ''Korytnica''| phone= |
        | tollfree= | fax=| hours= | price=| lastedit=2016-11-10|
        | content=The hike up is quite steep for an hour or two.}}""".stripMargin.replaceAll("\n", "")

    val lst = parseAll(listing("whatever"), txt).getOrElse(Listing("whatever"))

    lst.copy(id="") should equal (Listing("","whatever",Some("do"),
      Some("""''Prašivá'' or ''Veľká Chocuľa'' peaks in Low Tatras """),
      Some(48.8938),Some(19.3310),Some("48.8938,19.3310"),
      Some("""The hike up is quite steep for an hour or two."""
        .stripMargin.replaceAll("\n", ""))))

  }

  it should "parse listing when there are multiple inner 'pairs' with only one member in an entity" in {

    val txt =
      """{{see| name=Gedung Pakuan | alt=Huis van de Resident | url= | email=| address=Jl. Cicendo No 1 |
        |lat=-6.911792 | long=107.604771 | directions=northern end of Jl. Otto Iskandardinata| phone= |
        |tollfree= | fax=| hours= | price=| lastedit=2016-12-07| content=Built 1864-1867 as the new
        | residence of the resident of the residency of [[Parahyangan]], as its capital was moved
        | from [[Cianjur]] to Bandung. The Indies Empire style building is nowadays used as the
        | official residence of the governor of West Java province.}}""".stripMargin.replaceAll("\n", "")

    val lst = parseAll(listing("whatever"), txt).getOrElse(Listing("whatever"))

    // indexing adds a space because the parser is eating leading whitespace after it processes these inners

    lst.copy(id="") should equal (Listing("","whatever",Some("see"),
      Some("Gedung Pakuan "),
      Some(-6.911792),Some(107.604771),Some("-6.911792,107.604771"),
      Some("""Built 1864-1867 as the new
             | residence of the resident of the residency of Parahyangan , as its capital was moved
             | from Cianjur to Bandung. The Indies Empire style building is nowadays used as the
             | official residence of the governor of West Java province.""".stripMargin.replaceAll("\n", ""))))

  }
  it should "get a stream" in {
    val txt = "{{ this is {{ a test {{ to catch }} substrings }} and match }} }} brackets"
    WikiMediaParser.extractInnerExpression(txt,"{{","}}")
  }
}

