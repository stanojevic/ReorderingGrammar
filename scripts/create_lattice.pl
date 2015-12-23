#!/usr/bin/perl -w
use strict;

die "usage: $0   corpus   quasi_perm_file   kendall_table   openfst_loc   output_dir\n" unless @ARGV == 5;
# ./create_lattice.pl corpus.en out.QuasiPerm out.expectedKendall openfst_loc out.lattices


sub MAIN{
	my ($corpus, $quasi_perm_fn, $kendall_table, $openfst_loc, $output_dir) = @_;

	my $n_to_select = 100000000000000000000000000000;

	my $tmp_dir = $output_dir.'/tmp';
	my $fst_dir = $output_dir.'/weighted.openfst.lattice';

	mkdir($output_dir);
	mkdir($fst_dir);
	mkdir($tmp_dir);

	my $last_sent_id = undef;

	open my $quasi_fh, $quasi_perm_fn or die $!;
	open my $corpus_fh, $corpus or die $!;
	open my $kendall_fh, $kendall_table or die $!;

	open my $out_moses_fh, ">$output_dir/weighted.moses.lattice" or die $!;
	open my $out_permutation_fh, ">$output_dir/weighted.permutation.lattice" or die $!;

	my @perms = ();

	my @words = ();

	while(<$quasi_fh>){
		chomp;
		/sent (.+?) .*\|\|\| (.+)/;
		my $curr_sent_id = $1;
		my @nums = split / /, $2;
		my @newNums = map {($_ < 0)?$_+10000:$_} @nums;

		if(defined $last_sent_id and $curr_sent_id != $last_sent_id){
			process($last_sent_id, \@perms, $corpus_fh, $kendall_fh, $openfst_loc, $fst_dir, $tmp_dir, $out_moses_fh, $out_permutation_fh);

			@perms = ();
		}

		$last_sent_id = $curr_sent_id;

		if(scalar(@perms)<$n_to_select){
			push @perms, \@newNums;
		}
	}
	process($last_sent_id, \@perms, $corpus_fh, $kendall_fh, $openfst_loc, $fst_dir, $tmp_dir, $out_moses_fh, $out_permutation_fh);

	close $out_moses_fh;
	close $out_permutation_fh;

	close $quasi_fh;
	close $corpus_fh;
	close $kendall_fh;
}

sub process{
	my ($sent_id, $perms, $corpus_fh, $kendall_fh, $openfst_loc, $fst_dir, $tmp_dir, $out_moses_fh, $out_permutation_fh) = @_;

	print STDERR "sent $sent_id started\n";

	my @words = nextCorpusLine($corpus_fh);
	my $n = scalar(@words);
	createRawFST($sent_id, $perms, \@words, $tmp_dir);

	print STDERR "sent $sent_id minimizing...";
	minimize ($tmp_dir, $sent_id, $openfst_loc);
	print STDERR " done\n";

	print STDERR "sent $sent_id weighting...";
	my $table = nextKendallTable($kendall_fh, $n);
	createWeightedMinimalFST($sent_id, $fst_dir, $tmp_dir, $out_moses_fh, $out_permutation_fh, $table, $n);
	print STDERR " done\n";

	print STDERR "sent $sent_id finished\n";
}

sub createWeightedMinimalFST{
	my ($sent_id, $fst_dir, $tmp_dir, $out_moses_fh, $out_permutation_fh, $table, $n) = @_;
	my $fst_fn = "$tmp_dir/$sent_id.unweighted.min.fst";
	my %arcs = loadFST($fst_fn);
	my %state_seen_words; # state => index => 1

	for my $state (sort {$a <=> $b} keys %arcs){
		for my $arc (@{$arcs{$state}}){
			my $visited = $state_seen_words{$arc->{start}};
			my %visited = (defined $visited)? %$visited : ();
			$visited{$arc->{src_label}}=1;
			$state_seen_words{$arc->{end}} = \%visited;

			my $score = 0.0;
			for my $i (0..$n-1){
				next if $visited{$i};
				$score += $table->{$arc->{src_label}}->{$i};
			}
			$arc->{score}=$score;
		}
	}

	execute("cp $tmp_dir/$sent_id.isyms $fst_dir/$sent_id.isyms");
	execute("cp $tmp_dir/$sent_id.osyms $fst_dir/$sent_id.osyms");
	my $new_fst_fn = "$fst_dir/$sent_id.weighted.min.fst";
	my $finalState = 0;
	open my $out_fst_fh, ">$new_fst_fn" or die $!;
	for my $state (sort {$a <=> $b} keys %arcs){
		for my $arc (@{$arcs{$state}}){
			print $out_fst_fh join(" ", $arc->{start}, $arc->{end}, $arc->{src_label}, $arc->{tgt_label}, $arc->{score}),"\n";
			$finalState = $arc->{end} if $arc->{end}>$finalState;
		}
	}
	print $out_fst_fh join(" ", $finalState, 0.0),"\n";
	close $out_fst_fh;
	

	print $out_moses_fh "(";
	for my $state (sort {$a <=> $b} keys %arcs){
		print $out_moses_fh "(";
		for my $arc (@{$arcs{$state}}){
			print $out_moses_fh "(";
			print $out_moses_fh join(", ", "'".$arc->{tgt_label}."'", $arc->{score}, ($arc->{end}-$arc->{start}));
			print $out_moses_fh "),";
		}
		print $out_moses_fh "),";
	}
	print $out_moses_fh "),\n";

	print $out_permutation_fh "(";
	for my $state (sort {$a <=> $b} keys %arcs){
		print $out_permutation_fh "(";
		for my $arc (@{$arcs{$state}}){
			print $out_permutation_fh "(";
			print $out_permutation_fh join(", ", $arc->{src_label}, $arc->{score}, ($arc->{end}-$arc->{start}));
			print $out_permutation_fh "),";
		}
		print $out_permutation_fh "),";
	}
	print $out_permutation_fh "),\n";
}

sub loadFST{
	my ($fst_fn) = @_;

	my %arcs;

	open my $fh, $fst_fn or die "can't open $fst_fn $!";

	while(<$fh>){
		chomp;
		my @fields = split /\t+/;
		my $start = $fields[0];
		my $end = $fields[1];
		next if not defined $end;
		my $src_label = $fields[2];
		my $tgt_label = $fields[3];
		my $weight = $fields[4];
		$weight = 1 if not defined $weight;

		$arcs{$start} = [] unless exists $arcs{$start};

		push @{$arcs{$start}}, {start=>$start, end=>$end, src_label=>$src_label, tgt_label=>$tgt_label, weight=>$weight};
	}

	close $fh;

	return %arcs;
}

sub minimize{
	my ($fst_dir, $sent_id, $openfst_loc) = @_;
	execute("$openfst_loc/src/bin/fstcompile --isymbols=$fst_dir/$sent_id.isyms --osymbols=$fst_dir/$sent_id.osyms $fst_dir/$sent_id.unweighted.raw.fst $fst_dir/$sent_id.unweighted.raw.binary");
	execute("$openfst_loc/src/bin/fstdeterminize $fst_dir/$sent_id.unweighted.raw.binary $fst_dir/$sent_id.unweighted.determ.binary");
	execute("$openfst_loc/src/bin/fstminimize $fst_dir/$sent_id.unweighted.determ.binary $fst_dir/$sent_id.unweighted.min.binary");
	execute("$openfst_loc/src/bin/fstprint --isymbols=$fst_dir/$sent_id.isyms --osymbols=$fst_dir/$sent_id.osyms $fst_dir/$sent_id.unweighted.min.binary  $fst_dir/$sent_id.unweighted.min.fst");
}

sub execute{
	my ($cmd) = @_;
#print STDERR "START: $cmd\n";
	my $output = `$cmd 2>& 1`;
	print STDERR $output;
#print STDERR "DONE : $cmd\n";
}

sub nextCorpusLine{
	my ($corpus_fh) = @_;
	my $corpus_line = <$corpus_fh>;
	chomp $corpus_line;
	my @words = split / +/, $corpus_line;

	return @words;
}

sub nextKendallTable{
	my ($kendall_fh, $n) = @_;

	my $table_line = <$kendall_fh>;
	my @table_fields = split / +/, $table_line;
	my %table;
	for my $table_field (@table_fields){
		my @table_subfields = split /:/, $table_field;
		$table{$table_subfields[0]}{$table_subfields[1]}=$table_subfields[2];
	}
	for my $i (0..$n-1){
		for my $j (0..$n-1){
			if(not defined $table{$i}{$j}){
				$table{$i}{$j} = 0;
			}
		}
	}
	return \%table;
}

sub createRawFST{
	my ($sent_id, $perms, $words, $out_dir) = @_;

	my $perm_size = scalar(@{$perms->[0]});

	open my $fst_fh, ">$out_dir/$sent_id.unweighted.raw.fst" or die $!;

	my $perm_id = 0;
	for my $perm (@$perms){
		my @perm = @$perm;
		for my $i (0..$#perm){
			my $start_state = ($i==0)? $i : $perm_id*100+$i;
			my $end_state = ($i==$#perm)? $i+1 : $perm_id*100+$i+1;
			print $fst_fh join(" ", $start_state, $end_state, $perm[$i], $words->[$perm[$i]], 1.0), "\n";
		}
		$perm_id++;
	}

	print $fst_fh join(" ", $perm_size, 1.0), "\n";

	close $fst_fh;


	open my $isyms_fh, ">$out_dir/$sent_id.isyms" or die $!;
	open my $osyms_fh, ">$out_dir/$sent_id.osyms" or die $!;

	for my $i (0..$perm_size-1){
		print $isyms_fh          $i  , " $i\n";
		print $osyms_fh $words->[$i] , " $i\n";
	}
	
	close $isyms_fh;
	close $osyms_fh;
}


MAIN(@ARGV);

# notes
# ./convert_quasiperm_to_openfst.pl practice.out.QuasiPerm 10000 proba
#
# ./openfst-1.5.0/src/bin/fstcompile --isymbols=proba/0.isyms --osymbols=proba/0.osyms proba/0.fst proba/0.binary 
# ./openfst-1.5.0/src/bin/fstdraw --isymbols=proba/0.isyms --osymbols=proba/0.osyms proba/0.binary proba/0.dot
# dot -Tpng proba/0.dot > proba/0.png
# 
# ./openfst-1.5.0/src/bin/fstdeterminize proba/0.binary proba/0.determ.binary
# ./openfst-1.5.0/src/bin/fstprint --isymbols=proba/0.isyms --osymbols=proba/0.osyms proba/0.determ.binary  proba/0.determ.fst
# ./openfst-1.5.0/src/bin/fstdraw --isymbols=proba/0.isyms --osymbols=proba/0.osyms proba/0.determ.binary  proba/0.determ.dot
# dot -Tpng proba/0.determ.dot > proba/0.determ.png
#
# ./openfst-1.5.0/src/bin/fstminimize proba/0.determ.binary proba/0.min.binary
# ./openfst-1.5.0/src/bin/fstprint --isymbols=proba/0.isyms --osymbols=proba/0.osyms proba/0.min.binary  proba/0.min.fst
# ./openfst-1.5.0/src/bin/fstdraw --isymbols=proba/0.isyms --osymbols=proba/0.osyms proba/0.min.binary  proba/0.min.dot
# dot -Tpng proba/0.min.dot > proba/0.min.png
#
