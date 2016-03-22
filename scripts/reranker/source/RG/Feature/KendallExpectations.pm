package RG::Feature::KendallExpectations;
use Mouse;

sub features{
	my $self = shift;
	my ($nbest_entry) = @_;

	my $expKendall = $nbest_entry->{expKendall};
	my $prob = $nbest_entry->{prob};

	return {expKendall => $expKendall, prob => $prob};
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


__PACKAGE__->meta->make_immutable();

