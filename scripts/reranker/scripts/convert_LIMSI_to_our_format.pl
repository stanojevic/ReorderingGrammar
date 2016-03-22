#!/usr/bin/perl
use strict;




sub MAIN{
	my ($limsiFn, $maxLength, $sourceFn, $targetFn, $permutationFn, $alignFn) = @_;

	open my $fh, $limsiFn or die $!;
	open my $sourceFh, ">$sourceFn" or die $!;
	open my $targetFh, ">$targetFn" or die $!;
	open my $permutationFh, ">$permutationFn" or die $!;
	open my $alignFh , ">$alignFn"  or die $!;

	my @sentenceBuffer = ();
	my $targetIndex = 0;

	while(<$fh>){
		chomp;
		if(/^EOS$/){
			my ($srcSent, $tgtSent, $permutation, $align) = processBuffer(@sentenceBuffer);
			my @words = split / +/, $srcSent;
			if(@words<=$maxLength){
				print $sourceFh $srcSent, "\n";
				print $targetFh $tgtSent, "\n";
				print $permutationFh $permutation, "\n";
				print $alignFh  $align, "\n";
			}
			@sentenceBuffer = ();
			$targetIndex = 0;
		}else{
			my @fields = split / \|\|\| /;
			push @sentenceBuffer,
			       {
				source      => $fields[0],
				target      => $fields[1],
				sourcePOS   => $fields[2],
				sourceIndex => $fields[3],
				targetIndex => $targetIndex
			       };
			$targetIndex++;
		}

	}


	close $fh;

	close $sourceFh;
	close $targetFh;
	close $permutationFh;
	close $alignFh;
}

sub processBuffer{
	my @buffer = @_;
	my @alignments;
	my @sourceWords;
	my @targetWords;
	my @permutation;

	my $sourceWordRealIndex = 0;

	foreach my $phrase (sort {$a->{sourceIndex} <=> $b->{sourceIndex}} @buffer){
		if($phrase->{sourceIndex} < 0){
			next;
		}
		my @words = split / +/, $phrase->{source};

		$phrase->{sourceWordRealIndexStart} = $sourceWordRealIndex;
		
		foreach my $word (@words){
			push @sourceWords, $word;
			if($phrase->{target} ne 'NULL'){
				push @alignments, $sourceWordRealIndex."-".$phrase->{targetIndex};
			}
			$sourceWordRealIndex++;
		}
	}

	foreach my $phrase (sort {$a->{targetIndex} <=> $b->{targetIndex}} @buffer){
		if($phrase->{sourceIndex} >= 0){
			my @words = split / +/, $phrase->{source};
			for my $i (0..$#words){
				push @permutation, $phrase->{sourceWordRealIndexStart}+$i;
			}
		}

		my @words = split / +/, $phrase->{target};

		foreach my $word (@words){
			push @targetWords, $word unless $word eq 'NULL';
		}
	}

	my $srcString = join " ", @sourceWords;
	my $tgtString = join " ", @targetWords;
	my $permutationString = join " ", @permutation;
	my $alignString = join " ", @alignments;

	return ($srcString, $tgtString, $permutationString, $alignString);
}

die "usage: $0 original_limsi_file maxLength output_source_sentences output_target_sentences output_permutation output_fake_alignment" unless @ARGV == 6;

MAIN(@ARGV);

