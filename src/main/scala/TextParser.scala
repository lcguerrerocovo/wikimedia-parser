import scala.collection.immutable.Nil

/**
  * Created by luisguerrero on 2/14/17.
  */
trait TextParser {

  def parseText(text: String): List[String] = {
    if(text.isEmpty) Nil
    else {
      val (newText,index) = extractInnerExpression(text,"{{","}}")
      newText.replaceAll("\n","") :: parseText(text.substring(index))
    }
  }

  def subStrings(pattern: String, str: String) = {
    def subStrings(str: String, offset: Int): Stream[(Int,String)] = {
      val index = str.indexOf(pattern)
      if (index < 0) Stream.empty
      else {
        val slice = index + pattern.size
        Stream.cons((offset + slice, str.substring(slice)),subStrings(str.substring(slice),offset + slice))
      }
    }
    subStrings(str,0)
  }

  def extractInnerExpression(text: String, start: String, end: String): (String,Int) = {
    def findMatch(start: Stream[(Int,String)], end: Stream[(Int,String)], acc: Int): Int = {
      if(end.isEmpty) text.size
      else if(start.isEmpty && acc == 1) end.head._1
      else if(start.isEmpty) findMatch(start,end.tail,acc-1)
      else if(start.head._1 < end.head._1) findMatch(start.tail,end,acc+1)
      else if(start.head._1 > end.head._1 && acc != 1) findMatch(start,end.tail,acc-1)
      else end.head._1
    }
    val starts = subStrings(start,text)
    val ends = subStrings(end,text)
    val matchIndex = findMatch(starts,ends,0)

    if(matchIndex != text.size)
      (text.substring(starts.head._1 - start.size,matchIndex),matchIndex)
    else ("",text.size)
  }
}
