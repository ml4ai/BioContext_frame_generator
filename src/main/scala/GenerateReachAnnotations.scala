import java.io.File

import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.LazyLogging
import org.clulab.processors.Document
import org.clulab.processors.bionlp.BioNLPProcessor
import org.clulab.reach.ReachSystem
import org.clulab.reach.mentions.{BioEventMention, BioMention, BioTextBoundMention}
import org.clulab.struct.Interval
import org.clulab.utils.Serializer

import scala.io.Source

/** Represents a paper extraction */
case class PaperExtraction(sent:Int, interval:Interval, text:String, grounding:String)

/**
 * Generates REACH annotations for the papers given the version configured in build.sbt
 */
object GenerateReachAnnotations extends App with LazyLogging {

  val config = ConfigFactory.load("generateReachAnnotations")

  val paperFilesDir = config.getString("filesDirectory")
  val outputPath = config.getString("outputFile")

  def readTokens(path: String): Seq[Seq[String]] = {
    val lines = Source.fromFile(path).getLines().toList
    val tokens = lines map (_.split(" ").toList)
    tokens
  }

  def extractFrom(path: String): (Seq[BioMention], Document) = {
    val tokens = readTokens(path + "/sentences.txt")

    val doc = procAnnotator.mkDocumentFromTokens(tokens)
    doc.id = Some(path)
    doc.text = Some(tokens.map(_.mkString(" ")).mkString("\n"))
    procAnnotator.annotate(doc)

    (reachSystem extractFrom doc, doc)
  }

  // initialize ReachSystem
  logger.info(s"Loading BioNLPProcessor and REACH")
  val procAnnotator = new BioNLPProcessor()
  procAnnotator.annotate("test")
  val reachSystem = new ReachSystem(proc = Some(procAnnotator))

  val directories = for {f <- new File(paperFilesDir).listFiles(); if f.isDirectory} yield f

  val data =
    (for {dir <- directories.par} yield {
      val path = dir.getAbsolutePath
      val pmcid = dir.getName
      logger.info(s"Processing $pmcid ...")
      val (extractions, doc) = extractFrom(path)
      logger.info(s"Finished $pmcid")
      val tups =
        extractions collect {
          case e: BioEventMention =>
            PaperExtraction(e.sentence, e.tokenInterval, e.text, "Event")
          case m: BioTextBoundMention =>
            PaperExtraction(m.sentence, m.tokenInterval, m.text, m.grounding match { case Some(kb) => kb.nsId; case None => "" })
        }
      pmcid -> (tups, doc)
    }).seq


  logger.info(s"Saving output into $outputPath")
  Serializer.save(data, outputPath)

}
