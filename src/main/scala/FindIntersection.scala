import org.clulab.struct.Interval
import org.clulab.utils.Serializer

import scala.collection.mutable.ListBuffer

object FindIntersection extends App{

  def readSerializedExtractions(path:String):Map[String, Seq[PaperExtraction]] = {
    val data = Serializer.load[Array[(String, Seq[PaperExtraction])]](path)
    data.toMap
  }

  def readSerializedExtractions2(path:String):Map[String, Seq[PaperExtraction]] = {
    val data = Serializer.load[Seq[(String, Seq[PaperExtraction])]](path)
    data.toMap
  }

  def getEvents(data:Seq[PaperExtraction]):Seq[PaperExtraction] = data filter (_.grounding == "Event")

  def getIntersection(left:Seq[PaperExtraction], right:Seq[PaperExtraction]):Seq[PaperExtraction] = {
    // Small helper function
    def intersects(a:Interval, b:Interval):Boolean = {
      val (shorter, longer) = if (a.size <= b.size) (a, b) else (b, a)
      if(longer contains shorter)
        true
      else if(shorter.intersect(longer).nonEmpty)
        true
      else
        false
    }

    val sentences =
      ((left map (_.sent)) ++
      (right map (_.sent))).distinct

    val gpBySentLeft = left.groupBy(_.sent)
    val gpBySentRight = right.groupBy(_.sent)

    val intersection = ListBuffer[PaperExtraction]()

    for(sentIx <- sentences){
      if((gpBySentLeft contains sentIx) && (gpBySentRight contains sentIx)){
        for{
          l <- gpBySentLeft(sentIx)
          r <- gpBySentRight(sentIx)
        }{
          if(intersects(l.interval, r.interval))
            intersection += l
        }
      }
    }

    intersection.toList.distinct
  }

  val parsedAnnotations = readSerializedExtractions("parsed_annotations.ser")
  val reachExtractions = readSerializedExtractions2("results2020.ser")

  val parsedEvents = parsedAnnotations.mapValues(getEvents)
  val reachEvents = reachExtractions.mapValues(getEvents)

  for(pmcid <- parsedEvents.keys) yield {
    val parsed = parsedEvents(pmcid)
    val extracted = reachEvents(pmcid)
    val intersection = getIntersection(parsed, extracted)


    println(s"$pmcid Original size: ${parsed.size}, Extracted size: ${extracted.distinct.size}, Intersection: ${intersection.size}")
//    for(extraction <- reachEvents(pmcid))
//      println(extraction)
  }

//  for(extraction <- reachEvents("PMC4204162").distinct.sortBy(e => (e.sent, e.interval)))
//        println(extraction)

  val y = 0
}
