#!/usr/bin/perl
use strict;

use XML::Simple;

# die "usage: $0 fileWithParses\n" unless @ARGV == 1;

binmode STDIN, "utf-8";
binmode STDOUT, "utf-8";

my $parseFile = $ARGV[0];

sub MAIN{
	my $i = 0;
	while(<STDIN>){
# print "sent $i: ";
		$i++;
		chomp;
		my $pt = XMLin($_);

		my @newOrder = traverse($pt->{cons});

		my $final_punctuation = $pt->{content};
		if(defined $final_punctuation){
			$final_punctuation=~s/ +//g;
			push @newOrder, $final_punctuation;
		}

		print join(" ", @newOrder),"\n";
	}
}

sub traverse{
	my ($node) = @_;

	if(exists $node->{cons}){
		my %cons = %{$node->{cons}};
		while(exists $cons{cons}){
			%cons = %{$cons{cons}};
		}
		my $head = $node->{head};
		if(exists $cons{tok}){
			return $cons{tok}{content};
		}
# if($head eq "c7"){
# print "break here\n";
# }

		my @newWords;
		for my $consName (keys %cons){
# if($consName eq "c206"){
# 				print "break here\n";
# 			}
# 			print STDERR "entering $consName\n";
			if($consName eq $head){
				push @newWords, traverse($cons{$consName});
			}else{
				unshift @newWords, traverse($cons{$consName});
			}
# 			print STDERR "returned $consName\n";
		}
		return @newWords;
	}else{
		return $node->{tok}->{content};
	}
}

MAIN();


