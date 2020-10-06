import org.clulab.processors.Document
import org.clulab.utils.Serializer

package object utils {
  def readSerializedPaperAnnotations(path:String):Map[String, ManuallyAnnotatedData] = {
    val data = Serializer.load[Map[String, ManuallyAnnotatedData]](path)
    data
  }

  def readSerializedDocument(path:String):Map[String, Document] = {
    val data = Serializer.load[Array[(String, (Seq[PaperExtraction], Document))]](path)
    data.toMap.mapValues(_._2)
  }

  def readSerializedExtractions(path:String):Map[String, Seq[PaperExtraction]] = {
    val data = Serializer.load[Seq[(String, (Seq[PaperExtraction], Document))]](path)
    data.toMap.mapValues(_._1)
  }

  def getEvents(data:Seq[PaperExtraction]):Seq[PaperExtraction] = data filter (_.grounding == "Event")
}
