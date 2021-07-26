import ai.lum.nxmlreader.{NxmlReader, Preprocessor}

import java.io.{File, FileWriter}
import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.LazyLogging
import org.clulab.processors.bionlp.BioNLPProcessor
import org.clulab.reach.ReachSystem

import scala.collection.JavaConverters.collectionAsScalaIterableConverter

/**
  * Generates REACH annotations for the papers given the version configured in build.sbt
  */
object ConvertNXMLtoText extends App with LazyLogging {

  val config = ConfigFactory.load.getConfig("convertNXMLtoText")

  val paperFilesDir = config.getString("filesDirectory")
  val outputPath = config.getString("outputFile")

  // initialize ReachSystem
  logger.info(s"Loading BioNLPProcessor and REACH")

  val procAnnotator = new BioNLPProcessor()
  procAnnotator.annotate("test")
  val reachSystem = new ReachSystem(proc = Some(procAnnotator))

  val nxmlReader = new NxmlReader(
    config.getStringList("ignoreSections").asScala.toSet
  )

  for { f <- new File(paperFilesDir).listFiles() if f.isDirectory } yield {
    val path = f.getAbsolutePath
    val pmcid = f.getName
    logger.info(s"Processing $pmcid ...")
    val nxml_doc = nxmlReader.read(new File(path + "/" + pmcid + ".nxml"))
    val proc_doc = reachSystem.mkDoc(nxml_doc)
    val output_dir = new File(outputPath + "/" + pmcid)
    output_dir.mkdir()
    val file_writer = new FileWriter(
      new File(outputPath + "/" + pmcid + "/sentences.txt")
    )
    for (sent <- proc_doc.sentences) {
      file_writer.write(sent.getSentenceText + "\n")
    }
    file_writer.close()
    logger.info(s"Finished $pmcid")
  }

  logger.info(s"Saving output into $outputPath")
  //TODO
}
