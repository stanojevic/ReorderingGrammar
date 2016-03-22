package RG::Learner::Pairwise;
use Mouse;
use FindBin;

has 'params' => (is => 'ro', isa => 'Str');
# example params " -c 1"

sub train{
	my $self = shift;

	my ($trainFn, $outDir) = @_;

	my $liblinearRankBinDir = "$FindBin::Bin/dependencies/liblinear-ranksvm";

	my $params = $self->params;

	my $cmd = "$liblinearRankBinDir/train -s 8 $params $trainFn $outDir/model_ranking";

	$self->_run($cmd);
}

sub _run{
	my $self = shift;
	my ($cmd) = @_;

	print STDERR "running: $cmd\n";

	open my $pipe, "$cmd|" or die $!;

	while(<$pipe>){
		print STDERR $_;
	}

	close $pipe;
}

sub test{
	my $self = shift;

	my ($testFn, $outDir) = @_;

	my $liblinearRankBinDir = "$FindBin::Bin/dependencies/liblinear-ranksvm";

	my $cmd = "$liblinearRankBinDir/predict $testFn $outDir/model_ranking $outDir/test_result";

	$self->_run($cmd);

	return "$outDir/test_result";
}

__PACKAGE__->meta->make_immutable();

