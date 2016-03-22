#!/usr/bin/perl
use strict;

sub MAIN{
	my $max = 0;
	my $line = 0;
	while(<STDIN>){
		chomp;
		my $line_max = 0;
		for my $x (/, ?(\d+)\)/g){
			if($x>$line_max){
				$line_max = $x;
			}
		}
		if($line_max>$max){
			$max = $line_max;
		}
		print "sent $line max phrase length $line_max\n";
		$line++;
	}
	print "total max phrase length $max\n";
}

MAIN(@ARGV);
