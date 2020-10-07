# BioContext Frame Generator

## Overview
The code here uses the files derived from the files at https://github.com/ml4ai/BioContext_corpus_private/tree/master/data/papers/event_spans_Reach2016/

It will parse the sentences using the existing tokenization and generate a tsv file with the extracted features.

The output TSV can be read directly into pandas for further ETL.

## Notes
- This code doesn't do any down-sampling of negative examples.
- It doesn't group pairs either, deferring that step to modeling, however, it can be done really simple with pandas:
```python
grouped = frame.groupby(['EvtID', 'CtxID'])
``` 
- The dependency bigrams are not included, as they didn't prove useful on our previous study. However, it should be simple to add them if necessary.

## Configuration

The input and output files' paths are determined in the configuration file, read using Lightbend's config https://github.com/lightbend/config

Each script has its own dedicated section

## Scripts
Run the scripts in this order to get the features data frame as output

- `ParsePaperFiles`: Digest the input files with the manual annotations and generates a serialized file with the appropriate data structures
- `GenerateReachAnnotations`: Uses REACH to extract events and mentions from the `sentences.txt` files and serializes the output
- `GeneratePairs`: Generates the positive and negative pairs based on the outputs of the previous scripts. These will be used later for feature extraction
- `FeatureExtractor`: Generates the features of the pairs. Saves both a serialized version of the features and a TSV version to be read in python