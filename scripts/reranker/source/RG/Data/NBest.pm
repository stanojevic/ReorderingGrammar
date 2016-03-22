package RG::Data::NBest;

sub load{
	my ($nbestLoc, $unique) = @_; #expects out.QuasiPerm

	my %all_entries;
	my %all_permutations;

	my $currentSentId = 0;

	open my $fh, $nbestLoc or die $!;

	while(<$fh>){
		chomp;
		my ($props_str, $perm_str) = split / \|\|\| /;
		my @props = split / /, $props_str;
		my $sentId = $props[1];
		my $rankId = $props[3];
		my $expectation = $props[5];
		my $prob = $props[8];
		$prob=~s/.*://;
		my @perm = split / /, $perm_str;
		for my $i (0..$#perm){
			$perm[$i]+=10000 if $perm[$i] < 0;
		}
		my $perm_str = join(" ", @perm);

		my $entry = {
			expKendall	=> $expectation,
			prob		=> $prob,
			sentId		=> $sentId,
			rankId		=> $rankId,
			perm		=> \@perm,
			features	=> {}
			};

		if(not exists $all_entries{$sentId}){
			print STDERR "loading sent $sentId\n";
			$all_entries{$sentId} = [];
		}
		$all_permutations{$sentId} = {} unless exists $all_permutations{$sentId};

		if($unique and exists $all_permutations{$sentId}{$perm_str}){
			$all_permutations{$sentId}{$perm_str} += 1;
		}else{
			push @{$all_entries{$sentId}}, $entry;
			$all_permutations{$sentId}{$perm_str} = 1;
		}

	}

	close $fh;

	my @sorted = map {$all_entries{$_}} sort {$a <=> $b} keys %all_entries;
	return @sorted;
}

1;

