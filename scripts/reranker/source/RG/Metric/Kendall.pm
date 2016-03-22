package RG::Metric::Kendall;
use Mouse;
use Data::Dump qw/dump/;

sub scoreMonotone{
	my $self = shift;

	my ($refPerm) = @_;

	my @sysPerm;

	for my $i (0..scalar(@$refPerm)-1){
		push @sysPerm, $i;
	}

	return $self->score(\@sysPerm,$refPerm);
}

sub score{
	my $self = shift;

	my ($sysPerm, $refPerm) = @_;

	my @sysPerm;
	my @refPerm;

	if(scalar(@$sysPerm) != scalar(@$refPerm)){
		print STDERR "FUCK! sys:".dump($sysPerm)." ref:".dump($refPerm)."\n";
	}

	for my $i (0..scalar(@$sysPerm)-1){
		if($sysPerm->[$i]<0){
			push @sysPerm, $sysPerm->[$i]+10000;
		}else{
			push @sysPerm, $sysPerm->[$i];
		}
		if($refPerm->[$i]<0){
			push @refPerm, $refPerm->[$i]+10000;
		}else{
			push @refPerm, $refPerm->[$i];
		}
	}

	my %all_sys_skips;

	for my $i (0..$#sysPerm-1){
		for my $j ($i+1..$#sysPerm){
			my $skip = $sysPerm[$i].'-'.$sysPerm[$j];
			$all_sys_skips{$skip}=1;
		}
	}

	my $matchSkip = 0;
	my $allSkip = 0;

	for my $i (0..$#refPerm-1){
		for my $j ($i+1..$#refPerm){
			my $skip = $refPerm[$i].'-'.$refPerm[$j];
			if(exists $all_sys_skips{$skip}){
				$matchSkip++;
			}
			$allSkip++;
		}
	}

	if($allSkip <=0){
		print STDERR "wooops $allSkip sys:".dump($sysPerm)." ref:".dump($refPerm)."\n";
		return 0
	}else{
		return $matchSkip*1.0/$allSkip;
	}
}

__PACKAGE__->meta->make_immutable();

