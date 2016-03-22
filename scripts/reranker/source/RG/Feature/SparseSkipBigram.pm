package RG::Feature::SparseSkipBigram;
use Mouse;

has 'mappingLoc' => (
		is => 'ro',
		isa => 'Str');

has '_mappings' => (
		is => 'ro',
		isa => 'ArrayRef',
		lazy => 1,
		builder => '_mappings_opener',
		auto_deref => 1);

sub _mappings_opener{
	my $self = shift;

	my $mappings_fn = $self->mappingLoc;

	my @mappings;

	open my $fh, $mappings_fn or die $!;
	while(<$fh>){
		chomp;
		my @stuff = split / +/;
		push @mappings, \@stuff;
	}
	close $fh;

	return \@mappings;
}

sub addFeaturesNBest{
	my $self = shift;
	my ($nbest) = @_;
	my @nbest = @$nbest;

	for my $sent_i (0..$#nbest){
		my $entries = $nbest[$sent_i];
		for my $entry (@$entries){
			my $new_features = $self->features($entry);
			for my $featName (keys %$new_features){
				$entry->{features}->{$featName} = $new_features->{$featName};
			}
		}
	}
}

sub features{
	my $self = shift;
	my ($nbest_entry) = @_;

	my $mappings = $self->_mappings;

	my @perm = @{$nbest_entry->{perm}};
	my $sentId = $nbest_entry->{sentId};

	my @words = @{$mappings->[$sentId]};
	my @permuted_words;

	for my $i (0..$#perm){
		my $index = $perm[$i];
		$index = $index+10000 if $index<0;
		my $word = $words[$index];
		push @permuted_words, $word;
	}

	my %features;
	for my $i (0..$#permuted_words-1){
		for my $j ($i+1..$#permuted_words){
			my $desc = $permuted_words[$i].'_'.$permuted_words[$j];
			$features{$desc}++;
		}
	}

	return \%features;
}

__PACKAGE__->meta->make_immutable();

