
use strict;
use warnings;

my @containers;
open(my $fh, "<", "input.txt") or die "Error opening file: $!";
while (my $line = <$fh>) {
    chomp $line;
    push @containers, $line;
}
close($fh);

sub countCombinations {
    my ($containers, $target, $index) = @_;
    if ($target == 0) {
        return 1;
    }
    if ($target < 0 || $index >= scalar(@$containers)) {
        return 0;
    }
    return countCombinations($containers, $target - $containers->[$index], $index + 1) +
           countCombinations($containers, $target, $index + 1);
}

print countCombinations(\@containers, 150, 0) . "\n";
