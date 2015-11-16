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

Training
--------

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


Parameter                    | Description
---------------------------- | ----------------------------
--sourceFile corpus.src      |
--alignmentsFile alignments  |
--outputPrefix out_dir       |
--binarySplits 30            |
--narySplits 3               |
--useMinPhrases true         |
--nullAttachLeft false       |
--nullAttachRight true       |
--nullAttachTop false        |
--nullAttachBottom true      |
--maxRuleSum true            |
--hard_EM_best_K 1           |
--hard_EM_iter_start 11      |
--threads 30                 |
--threadBatchSize 100        |
--iterations 10              |
--version                    | prints current version
--help                       | prints available options



Output folder content 

file name                    | Description
---------------------------- | ----------------------------
filteredAlignments           |
filteredPos                  |
filteredSents                |
grammar_${I}                 |
grammar_${I}.dephrased       |
initGrammar                  |
nonSplittedGrammar           |



Testing
-------

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
--sentencesFile corpus.src                 |
--grammarFile $TMP_DIR/grammar             |
--outPermutedStringFN out.PermutedString   |
--outTreeFN out.Tree                       |
--outQuasiPermFN out.QuasiPerm             |
--kToExtract 10000                         |
--kToMBR 10000                             |
--kToOutput 1                              |
--doSampling true                          |
--lambdaPruning 0                          |
--grammarPruning 0                         |
--threads 30                               |
--flushingSize 40                          |
