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

# nohup perl -I./lib/lib/perl5 -I./source ./Train.pl \
# --nbest ../parse_result_nt09_grammar_10/out.QuasiPerm \
# --type pairwise \
# --out_dir model \
# --sprime_lm language_models/newsco.en2de.unfoldNULL.source.binary,language_models/newsco.en2de.unfoldNULL.source.source_pos.binary,language_models/newsco.en2de.unfoldNULL.source.sprimem_withpos_allwords.binary,language_models/newsco.en2de.unfoldNULL.source.clausetypes.binary,language_models/newsco.en2de.combined_POS.binary \
# --mappings dev_for_rerank/nt09.en2de.unfoldNULL.source,dev_for_rerank/nt09.en2de.unfoldNULL.source.source_pos,dev_for_rerank/nt09.en2de.unfoldNULL.source.sprimem_withpos_allwords,dev_for_rerank/nt09.en2de.unfoldNULL.source.clausetypes,dev_for_rerank/nt09.en2de.combined_POS.raw \
# --ref_permutation dev_for_rerank/nt09.en2de.unfoldNULL.permutation \
# --classifier_params '-c 1' \
# > log.rerank.train.std 2> log.rerank.train.err &

# --skip_bigram_mappings dev_for_rerank/nt09.en2de.unfoldNULL.source.source_pos \

sub MAIN{
	my ($opt, $usage) = describe_options(
		'%c %o <some-arg>',
		[ 'nbest|b=s', "nbest output of RG parser", { required => 1  } ],
		[ 'type|t=s',   "reranker type: (linear pairwise or RBF pointwise)",   { required =>1 } ],
		[ 'classifier_params|c=s',   "regularization for example",   { default => '-c 1' } ],
		[ 'metric|m=s',   "metric to minimize",   { default => 'kendall' } ],
		[ 'out_dir|o=s', "where to save model",   { required => 1 } ],
		[ 'ref_permutation|r=s', "reference permutation",   { required => 1 } ],
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

	my $referenceReorderings = RG::Data::ReferencePerm::load($opt->ref_permutation);

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
	my $trainFnUnscaled = "$outDir/train_data.unscaled";
	my $trainFnScaled = "$outDir/train_data.scaled";

############### new 

	my %all_indexed_features;

	for my $featureScorer (@featureScorers){
		$featureScorer->addFeaturesNBest(\@nbest);
	}

	my %featureMapping;
	my $nextFeatureIndex = 1;
	for my $sent_i (0..$#nbest){
		my $entries = $nbest[$sent_i];
		for my $entry (@$entries){
			for my $featName (keys %{$entry->{features}}){
				if(not exists $featureMapping{$featName}){
					$featureMapping{$featName} = $nextFeatureIndex;
					$nextFeatureIndex++;
				}
			}
		}
	}

	open my $trainFh, ">$trainFnUnscaled" or die $!;

	for my $sent_i (0..$#nbest){
		my $referenceReordering = $referenceReorderings->[$sent_i];
		my $qid = $sent_i+1;
		my $entries = $nbest[$sent_i];
		my $entry_id = 0;
		print STDERR "proc sent $sent_i\n";
		for my $entry (@$entries){
			$entry_id++;

			my $strDesc = "";

			my $score = $metricFunction->score($entry->{perm}, $referenceReordering);
			$strDesc.=$score;

			if($opt->type ne "pointwise"){
				$strDesc.=" qid:$qid";
			}
			for my $featName (sort {$featureMapping{$a} <=> $featureMapping{$b}} keys %{$entry->{features}}){
				my $featIndex = $featureMapping{$featName};
				my $featVal = $entry->{features}->{$featName};
				if(not $featVal){
					print STDERR "$featName $featIndex $featVal is the problem\n";
					$DB::single=1;
				}

				$strDesc.=" $featIndex:$featVal";
			}
			print $trainFh "$strDesc\n";
		}
	}

	close $trainFh;

	open my $mappingFh, ">$outDir/mapping" or die $!;
	for my $featName(sort {$featureMapping{$a} <=> $featureMapping{$b}} keys %featureMapping){
		print $mappingFh $featName.' '.$featureMapping{$featName}."\n";
	}
	close $mappingFh;

	`$FindBin::Bin/dependencies/liblinear-ranksvm/svm-scale -l 0 -s $outDir/scalingParams $trainFnUnscaled > $trainFnScaled`;

	$learner->train($trainFnScaled, $outDir);

############### new 
#
#	open my $trainFh, ">$trainFnUnscaled" or die $!;
#
#	my $nextFeatureIndex = 1;
#	my %featureMapping;
#
#	for my $sent_i (0..$#nbest){
#		my $referenceReordering = $referenceReorderings->[$sent_i];
#		my $qid = $sent_i+1;
#		my $entries = $nbest[$sent_i];
#		my $entry_id = 0;
#		for my $entry (@$entries){
#			print STDERR "proc sent $sent_i :: $entry_id\n";
#			$entry_id++;
#
#			my $strDesc = "";
#
#			my $score = $metricFunction->score($entry->{perm}, $referenceReordering);
#			$strDesc.=$score;
#
#			if($opt->type ne "pointwise"){
#				$strDesc.=" qid:$qid";
#			}
#
#			my %indexedFeatures;
#			for my $featureScorer (@featureScorers){
#				my $features = $featureScorer->features($entry);
#				for my $featName (keys %$features){
#					if(not exists $featureMapping{$featName}){
#						$featureMapping{$featName}=$nextFeatureIndex;
#						$nextFeatureIndex++;
#					}
#					$indexedFeatures{$featureMapping{$featName}}=$features->{$featName};
#				}
#			}
#			for my $index (sort {$a <=> $b} keys %indexedFeatures){
#				my $featVal = $indexedFeatures{$index};
#				$strDesc.=" $index:$featVal";
#			}
#			print $trainFh "$strDesc\n";
#		}
#	}
#
#	close $trainFh;
#
#	open my $mappingFh, ">$outDir/mapping" or die $!;
#	for my $featName(sort {$featureMapping{$a} <=> $featureMapping{$b}} keys %featureMapping){
#		print $mappingFh $featName.' '.$featureMapping{$featName}."\n";
#	}
#	close $mappingFh;
#
#	`$FindBin::Bin/dependencies/liblinear-ranksvm/svm-scale -l 0 -s $outDir/scalingParams $trainFnUnscaled > $trainFnScaled`;
#
#	$learner->train($trainFnScaled, $outDir);
}

MAIN(@ARGV)

