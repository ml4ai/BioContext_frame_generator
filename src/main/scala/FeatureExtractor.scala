import java.io.PrintWriter

import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.LazyLogging
import org.clulab.processors.Document
import org.clulab.struct.Interval
import org.clulab.utils.Serializer

case class PairFeatures(pmcid:String,
                        eventSentence:Int,
                        eventInterval:Interval,
                        contextSentence:Int,
                        contextInterval:Interval,
                        sentenceDistance:Int,
                        contextFrequency:Int,
                        contextGrounding:String,
                        isClosestContextClass:Boolean,
                        contextSentencePresentTense:Boolean,
                        eventSentencePresentTense:Boolean,
                        contextSentencePastTense:Boolean,
                        eventSentencePastTense:Boolean,
                        contextSentenceFirstPerson:Boolean,
                        eventSentenceFirstPerson:Boolean,
                        dependencyDistance:Int,
                        contextHasNegation:Boolean,
                        eventHasNegation:Boolean,
                        isContext:Boolean) { // True for the positive case
  override def toString: String = {
    Seq(s"$pmcid",
      s"$isContext",
      s"E${eventSentence}_${eventInterval.start}_${eventInterval.end}",
      s"$contextGrounding",
      s"$isClosestContextClass",
      s"$contextFrequency",
      s"$contextHasNegation",
      s"$contextSentenceFirstPerson",
      s"$contextSentencePastTense",
      s"$contextSentencePresentTense",
      s"$dependencyDistance",
      s"$eventHasNegation",
      s"$eventSentenceFirstPerson",
      s"$eventSentencePastTense",
      s"$eventSentencePresentTense",
      s"$sentenceDistance"
    ).mkString("\t")
  }
}

object PairFeatures {
  val headerRow: String = {
    Seq("PMCID",
      "label",
      "EvtID",
      "CtxID",
      "closesCtxOfClass",
      "context_frequency",
      "ctxNegationIntTail",
      "ctxSentenceFirstPerson",
      "ctxSentencePastTense",
      "ctxSentencePresentTense",
      "dependencyDistance",
      "evtNegationInTail",
      "evtSentenceFirstPerson",
      "evtSentencePastTense",
      "evtSentencePresentTense",
      "sentenceDistance").mkString("\t")
  }

  def mkTsv(rows: Iterable[PairFeatures]): String =
    (headerRow :: rows.map(_.toString).toList).mkString("\n")
}


object FeatureExtractor extends App with LazyLogging {

  val config = ConfigFactory.load().getConfig("featureExtractor")
  val inputPath = config.getString("inputPairs")
  val documentsPath = config.getString("inputExtractions")
  val outputPath = config.getString("outputFile")
  val outputTsvPath = config.getString("outputTsvFile")
  val paperFilesPath = config.getString("inputPaperData")

  def extractFeaturePairs(pmcid: String, pair: Pair, doc: Document,
                          counts: Map[String, Int],
                          locations: Map[String, Seq[Int]]) = {
    val event = pair.event
    val context = pair.contextMention

    // Helper functions to extract features
    def isSentencePresentTense(sent: Int, doc: Document): Boolean = {
      val sentence = doc.sentences(sent)
      val deps = sentence.dependencies.get
      val rootTags = deps.roots map sentence.tags.get

      // Return true if a root is a verb conjugated in present tense
      (rootTags contains "VB") ||
        (rootTags contains "VBP") ||
        (rootTags contains "VBZ") ||
        (rootTags contains "VBG")

    }

    def isSentencePastTense(sent: Int, doc: Document): Boolean = {
      val sentence = doc.sentences(sent)
      val deps = sentence.dependencies.get
      val rootTags = deps.roots map sentence.tags.get

      // Return true if a root is a verb conjugated in present tense
      (rootTags contains "VBD") ||
        (rootTags contains "VBN")
    }

    def isSentenceFirstPerson(sent: Int, doc: Document): Boolean = {
      val sentence = doc.sentences(sent)
      val tags = sentence.tags.get

      // Get the indices noun phrases
      val chunks = sentence.chunks.get
      val npIndices =
        chunks.zipWithIndex.collect { case (tag, ix) if tag.endsWith("-NP") => ix }

      // Get the POS tag in the noun phrases
      val npTags = npIndices map (ix => (ix, tags(ix)))

      // Find the personal pronouns and fetch their words
      val prps =
        npTags collect {
          case (ix, tag) if tag == "PRP" =>
            sentence.words(ix).toLowerCase
        }

      // If the personal pronoun is first person, return true
      (prps contains "i") || (prps contains "we") || (prps contains "us") || (prps contains "our")
    }

    def calculateDependencyDistance(ctx: PaperExtraction, evt: PaperExtraction): Int = {
      // If they appear in the same sentence, then compute the distance in dependency hops between them
      if (ctx.sent == evt.sent) {
        val deps = doc.sentences(event.sent).dependencies.get
        val allDistances =
          (for {
            i <- event.interval
            j <- ctx.interval
          }
            yield deps.shortestPath(i, j, ignoreDirection = true).size).filter(_ > 0)

        if (allDistances.nonEmpty)
          allDistances.min
        else
          doc.sentences(event.sent).size // If there is no path (I can't see why not) then return the theoretical max
      }
      // Otherwise the number of "roots" traversed plus the distance from each root to the head of the entity
      else {
        val depsEvt = doc.sentences(event.sent).dependencies.get
        val rootToEvt =
          (for {
            i <- event.interval
            j <- depsEvt.roots
          }
            yield depsEvt.shortestPath(i, j, ignoreDirection = true).size).filter(_ > 0)

        val ctxEvt = doc.sentences(context.sent).dependencies.get
        val rootToCtx =
          (for {
            i <- context.interval
            j <- ctxEvt.roots
          }
            yield ctxEvt.shortestPath(i, j, ignoreDirection = true).size).filter(_ > 0)

        val sentDistance = Math.abs(event.sent - context.sent)

        sentDistance + rootToEvt.min + rootToCtx.min
      }
    }

    def findNegationInTails(mention: PaperExtraction): Boolean = {
      val deps = doc.sentences(mention.sent).dependencies.get
      val labels =
        for {
          ix <- mention.interval
          (target, label) <- deps.getOutgoingEdges(ix)
          if !(mention.interval contains target)
        }
          yield label

      labels contains "neg"
    }

    // Here I compute the features
    val sentenceDistance = Math.abs(event.sent - context.sent)
    val contextCount = counts(context.grounding)
    val closestContextOf = {
      val minimumDistances =
        locations mapValues (sentences => sentences.map(s => Math.abs(s - event.sent)).min)
      val contextsByDistance = minimumDistances.toSeq.groupBy(_._2).mapValues(_.map(_._1).toSet)
      val shortestDistance = contextsByDistance.keys.min
      contextsByDistance(shortestDistance) contains context.grounding
    }
    val contextSentencePresentTense = isSentencePresentTense(context.sent, doc)
    val eventSentencePresentTense = isSentencePresentTense(event.sent, doc)

    val contextSentencePastTense = isSentencePastTense(context.sent, doc)
    val eventSentencePastTense = isSentencePastTense(event.sent, doc)

    val contextSentenceFirstPerson = isSentenceFirstPerson(context.sent, doc)
    val eventSentenceFirstPerson = isSentenceFirstPerson(event.sent, doc)

    val dependencyDistance = calculateDependencyDistance(context, event)

    val contextHasNegation = findNegationInTails(context)
    val eventHasNegation = findNegationInTails(event)

    PairFeatures(pmcid, event.sent, event.interval, context.sent, context.interval,
      sentenceDistance, contextCount, context.grounding, closestContextOf,
      contextSentencePresentTense, eventSentencePresentTense,
      contextSentencePastTense, eventSentencePastTense, contextSentenceFirstPerson, eventSentenceFirstPerson,
      dependencyDistance, contextHasNegation, eventHasNegation,
      pair.isContext)
  }

  // Read the paper files data
  logger.info(s"Reading paper files data from $paperFilesPath")
  val paperFilesData = utils.readSerializedPaperAnnotations(paperFilesPath)

  // Read the pairs
  logger.info(s"Reading pairs from $inputPath")
  val paperPairs = utils.readSerializedPairs(inputPath)

  // Read the serialized document objects
  logger.info(s"Reading serialized document objects from $documentsPath")
  val documents = utils.readSerializedDocument(documentsPath)

  // Read the mentions
  val paperExtractions = utils.readSerializedExtractions(documentsPath)

  // Compute the extraction counts for each paper
  val contextCounts = paperExtractions.mapValues {
    extractions =>
      extractions.groupBy(e => e.grounding).mapValues(_.size)
  }

  val contextIds = utils.generateValidContextIds(paperFilesData)

  val mentionLocations: Map[String, Map[String, Seq[Int]]] = paperExtractions.mapValues {
    extractions =>
      extractions filter (contextIds contains _.grounding) groupBy (_.grounding) mapValues (_.map(_.sent).sorted)
  }

  // Generate the features for each pair on all papers
  logger.info(s"Extracting features")
  val rows =
    (for {
      (pmcid, pairs) <- paperPairs.par
      pair <- pairs
    } yield {
      extractFeaturePairs(pmcid, pair, documents(pmcid), contextCounts(pmcid), mentionLocations(pmcid))
    }).seq


  // Save the features into a serialized file
  logger.info(s"Saving output to $outputPath")
  Serializer.save(rows, outputPath)

  // Generate the tsv file to be opened with pandas
  logger.info(s"Saving the features as a tsv to $outputTsvPath")
  val tsvContents = PairFeatures.mkTsv(rows)
  val pw = new PrintWriter(s"$outputTsvPath")
  pw.print(tsvContents)
  pw.close()
}


