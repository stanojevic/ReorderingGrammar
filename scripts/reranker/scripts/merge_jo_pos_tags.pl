#!/usr/bin/env perl
use strict;

die "usage: $0 file1 file2 file3\n" unless @ARGV==3;

open my $fh1, $ARGV[0] or die $!;
open my $fh2, $ARGV[1] or die $!;
open my $fh3, $ARGV[2] or die $!;

my $line_id = 0;

while(<$fh1>){
	my $line1 = $_;
	my $line2 = <$fh2>;
	my $line3 = <$fh3>;
	chomp $line1;
	chomp $line2;
	chomp $line3;
	my @things1 = split / +/,$line1;
	my @things2 = split / +/,$line2;
	my @things3 = split / +/,$line3;
	my @fieldsFinal;
	die "unequal lines $line_id\n" unless @things1==@things2;
	for my $i (0..$#things1){
		my @fields1 = split /\|/, $things1[$i];
		my @fields2 = split /\|/, $things2[$i];
		my @fields3 = split /\|/, $things3[$i];
		my $select1 = ($things1[$i]=~/\|/)? pop(@fields1) : "Nothing1";
		my $select2 = ($things2[$i]=~/\|/)? pop(@fields2) : "Nothing2";
		my $select3 = ($things3[$i]=~/\|/)? pop(@fields3) : "Nothing3";
		my $newPOS = "$select1+$select2+$select3";
		push @fieldsFinal, $newPOS;
	}
	print join(" ", @fieldsFinal), "\n";

	$line_id++;
}

close $fh1;
close $fh2;
close $fh3;
