import ai.lum.nxmlreader.NxmlReader

import java.io.{File, FileWriter}
import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.LazyLogging
import org.clulab.processors.Document
import org.clulab.processors.bionlp.BioNLPProcessor
import org.clulab.reach.ReachSystem
import org.clulab.reach.mentions.{BioEventMention, BioMention, BioTextBoundMention}
import org.clulab.struct.Interval
import org.clulab.utils.Serializer

import scala.collection.JavaConverters.collectionAsScalaIterableConverter
import scala.io.Source

/** Represents a paper extraction */
case class PaperExtraction(sent: Int, interval: Interval, text: String, grounding: String)

/**
  * Generates REACH annotations for the papers given the version configured in build.sbt
  */
object GenerateReachAnnotations extends App with LazyLogging {

  val config = ConfigFactory.load.getConfig("generateReachAnnotations")

  val inputFilesDir = config.getString("inputDirectory")
  val outputFilesDir = config.getString("outputDirectory")

  val nxmlReader = new NxmlReader(
    config.getStringList("ignoreSections").asScala.toSet
  )

  def extractFrom(path: String, pmcid: String): (Seq[BioMention], Document) = {
    val nxml_doc = nxmlReader.read(new File(path + "/" + pmcid + ".nxml"))
    val proc_doc = reachSystem.mkDoc(nxml_doc)
    (reachSystem.extractFrom(proc_doc), proc_doc)
  }

  // initialize ReachSystem
  logger.info(s"Loading BioNLPProcessor and REACH")
  val procAnnotator = new BioNLPProcessor()
  procAnnotator.annotate("test")
  val reachSystem = new ReachSystem(proc = Some(procAnnotator))

  val directories = for { f <- new File(inputFilesDir).listFiles(); if f.isDirectory } yield f

  for { dir <- directories.par } yield {
    val path = dir.getAbsolutePath
    val pmcid = dir.getName
    logger.info(s"Processing $pmcid ...")
    val (extractions, doc) = extractFrom(path, pmcid)
    logger.info(s"Finished $pmcid")

    // Create output directory for this paper
    val paper_output_dir = new File(outputFilesDir + "/" + pmcid)
    paper_output_dir.mkdir()

    // Write sentences to disk
    val text_output = new File(paper_output_dir.getAbsolutePath + "/sentences.txt")
    text_output.createNewFile()
    val text_output_writer = new FileWriter(text_output)
    for (sent <- doc.sentences) {
      text_output_writer.write(sent.words.mkString(" ") + "\n")
    }
    text_output_writer.close()

    // Write context mentions and event mentions to disk
    val event_output = new File(paper_output_dir.getAbsolutePath + "/event_intervals.txt")
    val context_output = new File(paper_output_dir.getAbsolutePath + "/mention_intervals.txt")
    event_output.createNewFile()
    context_output.createNewFile()
    val event_output_writer = new FileWriter(event_output)
    val context_output_writer = new FileWriter(context_output)
    var sent_idx = 0
    var event_sent = ""
    var context_sent = ""
    for (extract <- extractions.sortBy(ext => ext.sentence)) {
      // If there are no more extractions for this sentence, write the
      // results to file before moving on
      if (extract.sentence > sent_idx) {
        event_output_writer.write(sent_idx.toString + event_sent + "\n")
        context_output_writer.write(sent_idx.toString + context_sent + "\n")
        sent_idx = extract.sentence
        event_sent = ""
        context_sent = ""
      }

      extract match {
        case e: BioEventMention =>
          event_sent += " " + e.tokenInterval.start + "-"
          event_sent += e.tokenInterval.end + "-"
          event_sent += e.foundBy.split(",")(0)
        case m: BioTextBoundMention =>
          context_sent += " " + m.tokenInterval.start + "%"
          context_sent += (m.tokenInterval.end - 1) + "%"
          context_sent += m.text.split(' ').mkString("_") + "%"
          context_sent += (m.grounding match {
            case Some(kb) => kb.nsId;
            case None     => ""
          })
      }
    }

    event_output_writer.close()
    context_output_writer.close()
  }
  logger.info(s"Output saved into $outputFilesDir")
}
