package RG::Data::ReferencePerm;

sub load{
	my ($refPermFn) = @_;
	my @all_perms;

	open my $fh, $refPermFn or die "Can't open $refPermFn $!";

	while(<$fh>){
		chomp;
		my @perm = split / /;
		push @all_perms, \@perm;
	}

	close $fh;

	return \@all_perms;
}

1;
