package RG::Learner::Pointwise;
use Mouse;
use FindBin;

has 'params' => (is => 'ro', isa => 'Str');
# example params "-d 3 -c 1 -p 0.01"

sub train{
	my $self = shift;

	my ($trainFn, $outDir) = @_;

	my $libsvmRegBinDir = "$FindBin::Bin/dependencies/libsvm";

	my $params = $self->params;

	open my $pipe, "$libsvmRegBinDir/svm-train -s 3 -t 1 $params $trainFn $outDir/model_ranking|" or die $!;

	while(<$pipe>){
		print STDERR $_;
	}

	close $pipe;
}


__PACKAGE__->meta->make_immutable();

