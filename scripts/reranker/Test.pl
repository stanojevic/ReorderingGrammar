#!/usr/bin/env perl
use strict;
use warnings;
use FindBin;

use lib "$FindBin::Bin/lib/lib/perl5";
use lib "$FindBin::Bin/source";

use Getopt::Long::Descriptive;

use RG::Learner::Pairwise;
use RG::Learner::Pointwise;
use RG::Metric::Kendall;
use RG::Data::NBest;
use RG::Data::ReferencePerm;
use RG::Feature::SPrimeLM;
use RG::Feature::KendallExpectations;
use RG::Feature::SparseSkipBigram;
use Data::Dump qw/dump/;

# nohup perl -I./lib/lib/perl5 -I./source ./Test.pl \
# --nbest ../parse_result_nt10_grammar_10/out.QuasiPerm \
# --type pairwise \
# --out_dir model \
# --sprime_lm language_models/newsco.en2de.unfoldNULL.source.binary,language_models/newsco.en2de.unfoldNULL.source.source_pos.binary,language_models/newsco.en2de.unfoldNULL.source.sprimem_withpos_allwords.binary,language_models/newsco.en2de.unfoldNULL.source.clausetypes.binary,language_models/newsco.en2de.combined_POS.binary \
# --mappings dev_for_rerank/nt10.en2de.unfoldNULL.source,dev_for_rerank/nt10.en2de.unfoldNULL.source.source_pos,dev_for_rerank/nt10.en2de.unfoldNULL.source.sprimem_withpos_allwords,dev_for_rerank/nt10.en2de.unfoldNULL.source.clausetypes,dev_for_rerank/nt10.en2de.combined_POS.raw \
# --ref_permutation dev_for_rerank/nt10.en2de.unfoldNULL.permutation \
# > log.rerank.test.std 2> log.rerank.test.err &

# --skip_bigram_mappings dev_for_rerank/nt10.en2de.unfoldNULL.source.source_pos \

sub MAIN{
	my ($opt, $usage) = describe_options(
		'%c %o <some-arg>',
		[ 'nbest|b=s', "nbest output of RG parser", { required => 1  } ],
		[ 'type|t=s',   "reranker type: (linear pairwise or RBF pointwise)",   { required =>1 } ],
		[ 'classifier_params|c=s',   "regularization for example",   { default => '-c 1' } ],
		[ 'metric|m=s',   "metric to minimize",   { default => 'kendall' } ],
		[ 'out_dir|o=s', "where to save model",   { required => 1 } ],
		[ 'ref_permutation|r=s', "reference permutation",   { default => '' } ],
		[],
		[ 'sprime_lm|l=s@', "(binary) lang model of word order", { required => 1 } ],
		[ 'mappings|g=s@', "mapping", { required => 1 } ],
		[],
		[ 'skip_bigram_mappings|s=s@', "sparse skip bigram feature", { default => '' } ],
		[],
		[ 'help',       "print usage message and exit" ],
	);

	print($usage->text), exit if $opt->help;

	my $learner;
	if($opt->type eq "pairwise"){
		$learner = new RG::Learner::Pairwise (params => $opt->classifier_params)
	}elsif($opt->type eq "pointwise"){
		$learner = new RG::Learner::Pointwise(params => $opt->classifier_params)
	}else{
		die "bad value for type of classifier\n"
	}

	my $metricFunction;
	if($opt->metric eq "kendall"){
		$metricFunction = new RG::Metric::Kendall()
	}else{
		die "bad value for type of metric to optimize\n"
	}

	my $referenceReorderings;
	if($opt->ref_permutation){
		$referenceReorderings = RG::Data::ReferencePerm::load($opt->ref_permutation);
	}

	my @nbest = RG::Data::NBest::load($opt->nbest, 1);





	my $sprimeLMs = $opt->sprime_lm;
	my @sprimeLMs = split(/,/,join(',',@$sprimeLMs));
	my $mappings = $opt->mappings;
	my @mappings = split(/,/,join(',',@$mappings));

	my @featureScorers;
	for my $i (0..$#sprimeLMs){
		my $lmFeatureF = new RG::Feature::SPrimeLM(
			lmLoc=> $sprimeLMs[$i],
			mappingLoc=> $mappings[$i],
			id => "lm$i");
		push @featureScorers, $lmFeatureF;
	}

	my $skip_bigram_mappings = $opt->skip_bigram_mappings;
	if($skip_bigram_mappings){
		my @skip_bigram_mappings = split(/,/,join(',',@$skip_bigram_mappings));
		for my $i (0..$#skip_bigram_mappings){
			next if $skip_bigram_mappings[$i] eq '';
			my $skipFeatureF = new RG::Feature::SparseSkipBigram(
				mappingLoc=> $skip_bigram_mappings[$i]);
			push @featureScorers, $skipFeatureF;
		}
	}

	push @featureScorers, new RG::Feature::KendallExpectations();



	my $outDir = $opt->out_dir;
	mkdir $outDir;
	my $testFnUnscaled = "$outDir/test_data.unscaled";
	my $testFnScaled = "$outDir/test_data.scaled";

	my %featureMapping;
	open my $mappingFh, "<$outDir/mapping" or die $!;
	while(<$mappingFh>){
		chomp;
		my @fields = split / /;
		$featureMapping{$fields[0]}=$fields[1]*1;
	}
	close $mappingFh;

############### new 

	my %all_indexed_features;

	for my $featureScorer (@featureScorers){
		$featureScorer->addFeaturesNBest(\@nbest);
	}

	open my $testFh, ">$testFnUnscaled" or die $!;

	for my $sent_i (0..$#nbest){
		my $referenceReordering;
		if($referenceReorderings){
			$referenceReordering = $referenceReorderings->[$sent_i];
		}
		my $qid = $sent_i+1;
		my $entries = $nbest[$sent_i];
		my $entry_id = 0;
		print STDERR "proc sent $sent_i\n";
		for my $entry (@$entries){
			$entry_id++;

			my $strDesc = "";

			my $score;
			if($referenceReorderings){
				$score = $metricFunction->score($entry->{perm}, $referenceReordering);
			}else{
				$score = 1;
			}
			$strDesc.=$score;
			$entry->{kendallReal} = $score;

			if($opt->type ne "pointwise"){
				$strDesc.=" qid:$qid";
			}
			for my $featName (sort {$featureMapping{$a} <=> $featureMapping{$b}} keys %{$entry->{features}}){
				my $featIndex = $featureMapping{$featName};
				my $featVal = $entry->{features}->{$featName};

				$strDesc.=" $featIndex:$featVal";
			}
			print $testFh "$strDesc\n";
		}
	}

	close $testFh;

	`$FindBin::Bin/dependencies/liblinear-ranksvm/svm-scale -l 0 -s $outDir/scalingParams $testFnUnscaled > $testFnScaled`;

	my $resultFn = $learner->test($testFnScaled, $outDir);

	open my $resultFh, $resultFn or die $!;

	for my $sent_i (0..$#nbest){
		my $entries = $nbest[$sent_i];
		for my $entry (@$entries){
			my $line = <$resultFh>;
			chomp $line;
			my $score = $line+0.0;
			$entry->{rerankScore} = $score;
		}
	}

	close $resultFh;


	###### analysis of results #####
	# 1. avg number of hypotheses to rerank
	# 2. avg oracle (best kendall tau)
	# 3. avg reranked kendall
	# 4. avg not reranked kendall
	# 5. avg score of monotone hypothesis
	
	my $sentCount = scalar(@nbest);

	my $hypCount = 0;
	my $oracleKendallCount = 0;
	my $rerankedKendallCount = 0;
	my $nonRerankedKendallCount = 0;
	my $monotoneKendallCount = 0;
	my @bestPermutations;

	for my $sent_i (0..$#nbest){
		my $entries = $nbest[$sent_i];
		if($referenceReorderings){
			my $referenceReordering = $referenceReorderings->[$sent_i];
			$monotoneKendallCount += $metricFunction->scoreMonotone($referenceReordering);
		}
		my $bestKendall = 0.0;

		my $bestRerankScore = undef;
		my $bestRerankKendall = undef;
		my $bestRerankPerm = undef;

		my $bestNonRerankScore = undef;
		my $bestNonRerankKendall = undef;

		for my $entry (@$entries){
			$hypCount ++;

			if($entry->{kendallReal} > $bestKendall){
				$bestKendall = $entry->{kendallReal};
			}
			if(not $bestRerankScore or $entry->{rerankScore}>$bestRerankScore){
				$bestRerankScore = $entry->{rerankScore};
				$bestRerankKendall = $entry->{kendallReal};
				$bestRerankPerm = $entry->{perm};
			}
			if(not $bestNonRerankScore or $entry->{expKendall} > $bestNonRerankScore){
				$bestNonRerankScore = $entry->{expKendall};
				$bestNonRerankKendall = $entry->{kendallReal};
			}
		}
		$oracleKendallCount += $bestKendall;
		$rerankedKendallCount += $bestRerankKendall;
		$nonRerankedKendallCount += $bestNonRerankKendall;
		push @bestPermutations, $bestRerankPerm;
	}

	$hypCount /= $sentCount+0.0;
	print "avgHypCount=$hypCount\n";
	$oracleKendallCount /= $sentCount+0.0;
	print "avgOracleKendall=$oracleKendallCount\n";
	$rerankedKendallCount /= $sentCount+0.0;
	print "avgRerankKendall=$rerankedKendallCount\n";
	$nonRerankedKendallCount /= $sentCount+0.0;
	print "avgNonRerankKendall=$nonRerankedKendallCount\n";
	$monotoneKendallCount /= $sentCount+0.0;
	print "avgNoReorderingKendall=$monotoneKendallCount\n";

	open my $outResultFh, ">$outDir/test_bestPermutationsAfterReranking" or die $!;

	for my $perm (@bestPermutations){
		print $outResultFh join(" ", @$perm),"\n";
	}

	close $outResultFh;
}

MAIN(@ARGV)

