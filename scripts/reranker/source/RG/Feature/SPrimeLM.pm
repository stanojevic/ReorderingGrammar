package RG::Feature::SPrimeLM;
use Mouse;
use FindBin;

#use Expect;

has 'countcount' => (
		is => 'rw',
		isa => 'Int',
		default => 0);

has 'id' => (
	is => 'ro',
	isa => 'Str');

has 'lmLoc' => (
		is => 'ro',
		isa => 'Str');

has 'mappingLoc' => (
		is => 'ro',
		isa => 'Str');

has '_mappings' => (
		is => 'ro',
		isa => 'ArrayRef',
		lazy => 1,
		builder => '_mappings_opener',
		auto_deref => 1);

#has '_pipe' => (
#		is => 'ro',
#		lazy => 1,
#		builder => '_pipe_opener');

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

#sub _pipe_opener{
#	my $self = shift;
#
#	my $kenlmBinDir = "$FindBin::Bin/dependencies/kenlm";
#	my $lmLoc = $self->lmLoc;
#
#	my $query = "$kenlmBinDir/bin/query -s $lmLoc";
#	#$DB::single = 1;
#	my $exp = Expect->spawn($query);
#
#	return $exp;
#}

sub addFeaturesNBest{
	my $self = shift;
	my ($nbest) = @_;
	my @nbest = @$nbest;

	my $mappings = $self->_mappings;
	my $id = $self->id;
	my $desc = "rawLMscore_$id";

	my $tmpFn = "tmpFile";
	open my $fh, ">$tmpFn" or die $!;
	for my $sent_i (0..$#nbest){
		my $entries = $nbest[$sent_i];
		for my $entry (@$entries){
			my @perm = @{$entry->{perm}};
			my $sentId = $entry->{sentId};

			my @words = @{$mappings->[$sentId]};
			my @permuted_words;

			for my $i (0..$#perm){
				my $index = $perm[$i];
				$index = $index+10000 if $index<0;
				my $word = $words[$index];
				push @permuted_words, $word;
			}

			print $fh join(" ", @permuted_words), "\n";
		}
	}
	close $fh;

	my $kenlmBinDir = "$FindBin::Bin/dependencies/kenlm";
	my $lmLoc = $self->lmLoc;

	my $query = "$kenlmBinDir/bin/query -s $lmLoc < $tmpFn >$tmpFn.out 2> /dev/null";
	`$query`;
	open my $fhIn, "$tmpFn.out" or die $!;
	for my $sent_i (0..$#nbest){
		my $entries = $nbest[$sent_i];
		for my $entry (@$entries){
			my $line = <$fhIn>;
			$line=~/Total: (-\d+\.?\d+)/;
			my $score = $1+0.0;
			$entry->{features}->{$desc}=$score;
		}
	}
	close $fhIn;
}

#sub features{
#	my $self = shift;
#	my ($nbest_entry) = @_;
#
#	my $exp = $self->_pipe;
#
#	my $mappings = $self->_mappings;
#
#	my @perm = @{$nbest_entry->{perm}};
#	my $sentId = $nbest_entry->{sentId};
#
#	my @words = @{$mappings->[$sentId]};
#	my @permuted_words;
#
#	for my $i (0..$#perm){
#		my $index = $perm[$i];
#		$index = $index+10000 if $index<0;
#		my $word = $words[$index];
#		push @permuted_words, $word;
#	}
#
#	print $exp join(" ",@permuted_words), "\r\n";
#	#$DB::single = 1;
#	my $res;
#	my $count = 0;
#	do{
#		$res = <$exp>;
#		print STDERR "ddd' $res 'ddd\n";
#		$count++;
#	}until $res=~/Total: (-\d+\.?\d+)/;
#	my $score = $1+0.0;
#
#	my $countcount = $self->countcount;
#	print STDERR "$countcount ::$count ::$score\n";
#	$self->countcount($countcount+1);
#
#	my $id = $self->id;
#	my $desc = "rawLMscore_$id";
#	return {$desc => $score};
#}

#sub DEMOLISH{
#	my $self = shift;
#
#	my $exp = $self->_pipe;
#
#	$exp->hard_close();
#}


__PACKAGE__->meta->make_immutable();

