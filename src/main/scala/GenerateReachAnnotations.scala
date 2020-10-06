import java.io.File

import org.clulab.processors.Document
import org.clulab.processors.bionlp.BioNLPProcessor
import org.clulab.reach.ReachSystem
import org.clulab.reach.mentions.{BioEventMention, BioMention, BioTextBoundMention, CorefEventMention}
import org.clulab.struct.Interval
import org.clulab.utils.Serializer

import scala.io.Source

case class PaperExtraction(sent:Int, interval:Interval, text:String, grounding:String)

object GenerateReachAnnotations extends App {

  def readTokens(path:String):Seq[Seq[String]] = {
    val lines = Source.fromFile(path).getLines().toList
    val tokens = lines map (_.split(" ").toList)
    tokens
  }

  def extractFrom(path:String):(Seq[BioMention], Document) = {
    val tokens = readTokens(path + "/sentences.txt")

    val doc = procAnnotator.mkDocumentFromTokens(tokens)
    doc.id = Some(path)
    doc.text = Some(tokens.map(_.mkString(" ")).mkString("\n"))
    procAnnotator.annotate(doc)

    (reachSystem extractFrom doc, doc)
  }

  // initialize ReachSystem
  val procAnnotator = new BioNLPProcessor()
  procAnnotator.annotate("test")
  val reachSystem = new ReachSystem(proc = Some(procAnnotator))

  val papersDir = "/Users/enrique/github/BioContext_corpus_private/data/papers/event_spans_Reach2016/"

  val directories = for { f <- new File(papersDir).listFiles() ; if f.isDirectory } yield f

  val data =
    (for {dir <- directories.par} yield {
      val path = dir.getAbsolutePath
      val pmcid = dir.getName
      println(s"Processing $pmcid ...")
      val (extractions, doc) = extractFrom(path)
      println(s"Finished $pmcid")
      val tups =
        extractions collect {
          case e:BioEventMention =>
            PaperExtraction(e.sentence, e.tokenInterval, e.text, "Event")
          case m:BioTextBoundMention =>
            PaperExtraction(m.sentence, m.tokenInterval, m.text, m.grounding match { case Some(kb) => kb.nsId; case None => ""})
        }
      pmcid -> (tups, doc)
    }).seq


//  val x = 0
  Serializer.save(data, "results2016.ser")

}
