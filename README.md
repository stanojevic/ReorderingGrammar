Reordering Grammar (RG)
=======================

This is a software for preordering (among many other things) in statistical machine translation.
The approach it implements is described in the following paper:

Reordering Grammar Induction <br />
Miloš Stanojević and Khalil Sima'an <br />
EMNLP 2015 <br />
[pdf](http://www.aclweb.org/anthology/D15-1005)
[bib](http://www.aclweb.org/anthology/D/D15/D15-1005.bib)

In short, it finds a context free grammar in:

* unsupervised
* probabilistic
* hierarchical

way by using only source side and word alignments to the target side.

There are many latent variables (latent bracketing and latent state splits of non-terminals)
so it is also has its specialized parser for finding the best reordering of the input string give the learned grammar.

Naturally, it has a training phase and testing phase. Both of them are described bellow.

# Training

To use RG it is needed to add the jar file to the classpath (with `-cp` parameter for example) and then call the appropriate class.
The class used for training is `grammar.reordering.Train` while for testing it is `grammar.reordering.Parse`.

Usually requires quite a bit of RAM memory and many CPUs (setting bellow is with 200GB RAM and 32 CPUs).
Memory is specified with standard Java parameter `-Xmx` and number of CPUs with `--threads` parameter.

Bellow is an example of a command for training reordering grammar.<br />
These parameters will be explained in this text.
There are many more other parameters (available trough `--help`) but are not tested or explained here.<br />
In case you run reordering grammar (with tested or no tested settings) it would be great if you let me know how it worked.

```
java -Xmx200G -cp rg*.jar grammar.reordering.Train \
              --sourceFile corpus.src \
              --alignmentsFile alignments \
              --outputPrefix out_dir \
              \
              --binarySplits 30 \
              --narySplits 3 \
              \
              --useMinPhrases true \
              \
              --nullAttachLeft false \
              --nullAttachRight true \
              --nullAttachTop false \
              --nullAttachBottom true \
              \
              --maxRuleSum true \
              --hard_EM_best_K 1 \
              --hard_EM_iter_start 11 \
              \
              --threads 30 \
              --threadBatchSize 100 \
              --iterations 10
```


Parameter                      | Description
------------------------------ | ----------------------------
--sourceFile corpus.src        | source side of parallel data
--alignmentsFile alignments    | alignment file
--outputPrefix out_dir         | directory in which to store output files
--binarySplits 30              | number of state splits per each binary non-terminal
--narySplits 3                 | number of state splits per each n-ary (n>2) non-terminal
--useMinPhrases true           | should smallest units of preordering be minimal phrases
--nullAttachLeft false         | should unaligned word be attached to the left
--nullAttachRight true         | should unaligned word be attached to the left
--nullAttachTop false          | should unaligned word be attached to the highest node it can
--nullAttachBottom true        | should unaligned word be attached to the lowest node it can
--maxRuleSum true              | NOT TESTED should treebank extraction (or hard-EM) be based on maxRuleSum
--maxRuleProduct false         | NOT TESTED should treebank extraction (or hard-EM) be based on maxRuleProduct
--hard_EM_best_K 1             | how many derivations to extract for hard-EM or treebank output
--hard_EM_iter_start 11        | on which iteration does hard-EM kick in (usually the last iteration, if at all)
--threads 30                   | how many CPUs to use for training
--threadBatchSize 100          | how big are the batches for each thread (100 is a reasonable setting)
--iterations 10                | how many iterations in total (both soft and hard EM)
--wordClassFile pos_tagged     | NOT TESTED file with POS tags that would be used as preterminals (each word of the source file replaced by its pos tag/cluster id)
--maxTrainingDataSize 10000000 | filtering argument for amount of training sentences to be used
--maxSentLength 50             | filtering argument for maximum sentence length
--maxAllowedArity 5            | filtering argument for maximum arity that is supported (don't try >5)
--canonicalOnly false          | should only one tree be used instead of forest
--rightBranching false         | if canonicalOnly is true then should it be left-branching tree (setting this parameter as false) or right-branching tree (setting this parameters as true)
--version                      | prints current version
--help                         | prints available options

**Note on null word attachment**: where you attach them is usually influenced by the syntax of the source language.
If the source language is mostly right branching (or in other words if it is head-initial) you should attach unaligned words to the right.
This is the recommended setting for most Indo-European languages.
Languages that are head-final (such as Japanese) would probably be parsed better if you attach unaligned words to the left.
Whether unaligned words should be attached high or low is something I'm not sure yet.
Attaching high is more justified I think but I didn't test that yet (if you do let me know). Attaching low works fine.

**Note on minimal phrases**: I didn't try setting without them.

**Note on hard-EM/treebank extraction**: This is an optional step.
It can be useful if you want to lower the size of the grammar or sharpen the distribution (not necessarily a good thing).

**Note on POS tags**: I still didn't test option `--wordClassFile`.
Maybe it will work in training but not in testing or the other way around.
You've been warned.

### Output folder content 

file name                    | Description
---------------------------- | ----------------------------
filteredSents                | After initial filtering of the data by filtering arguments this is what is left for training from sentences
filteredAlignments           | --||-- from alignments
filteredPos                  | --||-- from pos tags
nonSplittedGrammar           | Initial version of the grammar before training (without splits)
initGrammar                  | Initial version of the grammar before training (with splits)
grammar_${i}                 | grammar after iteration ${i} (if minPhrases are used then each minPhrase is a fake word in this grammar)
grammar_${i}.dephrased       | --||-- except that fake words that are minPhrases are expanded to be proper rules. This is the grammar you should use in test phase

**Note that the file names might change in the future**


# Testing

```
java -Xmx200G -cp rg*.jar grammar.reordering.Parse \
              --sentencesFile $TMP_DIR/corpus.$SRC_LANG \
              --grammarFile $TMP_DIR/grammar \
              \
              --outPermutedStringFN out.PermutedString \
              --outTreeFN out.Tree \
              --outQuasiPermFN out.QuasiPerm \
              \
              --kToExtract 10000 \
              --kToMBR 10000 \
              --kToOutput 1 \
              --doSampling true \
              \
              --lambdaPruning 0 \
              --grammarPruning 0 \
              \
              --threads 30 \
              --flushingSize 40
```

Parameter                                  | Description
------------------------------------------ | ----------------------------
--sentencesFile corpus.src                 | sentences to preorder
--grammarFile grammar.dephrased            | learned grammar
--outPermutedStringFN out.PermutedString   | preordered source sentences
--outTreeFN out.Tree                       | output best derivations for each sentence
--outQuasiPermFN out.QuasiPerm             | output the permutations (unaligned words are represented by their index - 10000)
--kToExtract 10000                         | size of the sample
--kToMBR 10000                             | size of the subset of the sample that will be used for MBR
--kToOutput 1                              | how many least risky derivations to print out
--doSampling true                          | true=MC sampling false=viterbi
--lambdaPruning 0                          | 0=no pruning >0 means exponentially prune more
--grammarPruning 0                         | treshold for rules which to leave out from the grammar
--threads 30                               | number of threads used in decoding
--flushingSize 40                          | after how many sentences to synchronize threads and output what is parsed so far
