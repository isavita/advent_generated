
use strict;
use warnings;

open(my $fh, '<', 'input.txt') or die "Error opening file: $!";
my @containers;
while (my $line = <$fh>) {
    chomp $line;
    push @containers, $line;
}
close($fh);

my ($minCount, $ways) = (0, 0);
findCombinations(\@containers, 150, 0, 0, \$minCount, \$ways);
print "$ways\n";

sub findCombinations {
    my ($containers, $target, $index, $count, $minCount, $ways) = @_;
    if ($target == 0) {
        if ($$minCount == 0 || $count < $$minCount) {
            $$minCount = $count;
            $$ways = 1;
        } elsif ($count == $$minCount) {
            $$ways++;
        }
        return;
    }
    if ($target < 0 || $index >= scalar(@$containers)) {
        return;
    }
    # Include current container
    findCombinations($containers, $target - $containers->[$index], $index + 1, $count + 1, $minCount, $ways);
    # Exclude current container
    findCombinations($containers, $target, $index + 1, $count, $minCount, $ways);
}
