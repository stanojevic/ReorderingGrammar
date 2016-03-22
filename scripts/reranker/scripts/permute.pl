#!/usr/bin/env perl
use strict;

die "usage: $0 permutation_file source_file\n" unless @ARGV==2;

my ($perm_fn, $source_fn) = @ARGV;

open my $perm_fh, $perm_fn or die $!;
open my $source_fh, $source_fn or die $!;

while(<$perm_fh>){
	chomp;
	my $perm_line = $_;
	my $source_line = <$source_fh>;
	chomp $source_line;

	my @perm = map {($_<0)?$_+10000:$_} split / +/, $perm_line;
	my @words = split / +/, $source_line;

	die "ups\n" unless @perm == @words;

	my @new_order;

	for my $i (0..$#perm){
		push @new_order, $words[$perm[$i]];
	}

	print join(" ", @new_order), "\n";
}

close $perm_fh;
close $source_fh;

print STDERR "successfully permuted $source_fn\n";

