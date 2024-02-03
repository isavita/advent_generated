
use strict;
use warnings;

sub readInput {
    open(my $fh, "<", "input.txt") or die "Cannot open file: $!";
    my $totalElves = <$fh>;
    close($fh);
    return $totalElves;
}

sub findWinningElf {
    my ($totalElves) = @_;
    my $highestPowerOfTwo = 1;
    while ($highestPowerOfTwo * 2 <= $totalElves) {
        $highestPowerOfTwo *= 2;
    }
    return ($totalElves - $highestPowerOfTwo) * 2 + 1;
}

my $totalElves = readInput();
my $winner = findWinningElf($totalElves);
print "$winner\n";
