import java.io.{File, FileWriter}
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
case class PaperExtraction(sent: Int, interval: Interval, text: String, grounding: String)

/**
  * Generates REACH annotations for the papers given the version configured in build.sbt
  */
object GenerateReachAnnotations extends App with LazyLogging {

  val config = ConfigFactory.load.getConfig("generateReachAnnotations")

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

  val directories = for { f <- new File(paperFilesDir).listFiles(); if f.isDirectory } yield f

  val start_time = System.nanoTime()
  val data =
    (for { dir <- directories.par } yield {
      val path = dir.getAbsolutePath
      val pmcid = dir.getName
      logger.info(s"Processing $pmcid ...")
      val (extractions, doc) = extractFrom(path)
      logger.info(s"Finished $pmcid")
      val tups =
        extractions collect {
          case e: BioEventMention =>
            PaperExtraction(e.sentence, e.tokenInterval, e.foundBy, "Event")
          case m: BioTextBoundMention =>
            PaperExtraction(
              m.sentence,
              m.tokenInterval,
              m.text,
              m.grounding match { case Some(kb) => kb.nsId; case None => "" }
            )
        }

      val event_output = new File(paperFilesDir + "/" + pmcid + "/event_intervals.txt")
      val context_output = new File(paperFilesDir + "/" + pmcid + "/mention_intervals.txt")
      event_output.createNewFile()
      context_output.createNewFile()
      val event_output_writer = new FileWriter(event_output)
      val context_output_writer = new FileWriter(context_output)
      var sent_idx = 0
      var event_sent = ""
      var context_sent = ""
      for (tup <- tups.sortBy(extraction => extraction.sent)) {
        // If there are no more extractions for this sentence, write the
        // results to file before moving on
        if (tup.sent > sent_idx) {
          event_output_writer.write(sent_idx.toString + event_sent + "\n")
          context_output_writer.write(sent_idx.toString + context_sent + "\n")
          sent_idx = tup.sent
          event_sent = ""
          context_sent = ""
        }

        if (tup.grounding == "Event") {
          event_sent += " " + tup.interval.start + "-" + tup.interval.end
          event_sent += "-" + tup.text.split(",")(0)
        } else {
          context_sent += " " + tup.interval.start + "%" + (tup.interval.end - 1) + "%"
          context_sent += tup.text.split(' ').mkString("_") + "%" + tup.grounding
        }
      }
      event_output_writer.close()
      context_output_writer.close()
      pmcid -> (tups, doc)
    }).seq
  val end_time = System.nanoTime()
  println("Elapsed time: " + (end_time - start_time) * 1e9 + "s")
  logger.info(s"Saving output into $outputPath")
  Serializer.save(data, outputPath)

}
