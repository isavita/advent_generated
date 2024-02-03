
use strict;
use warnings;

open(my $fh, '<', 'input.txt') or die "Error opening file: $!";
my @adapters;

while (my $line = <$fh>) {
    chomp $line;
    push @adapters, $line;
}

close($fh);

@adapters = sort {$a <=> $b} @adapters;
my %joltDifferences = (3 => 1);
my $previousJoltage = 0;

foreach my $adapter (@adapters) {
    my $diff = $adapter - $previousJoltage;
    $joltDifferences{$diff}++;
    $previousJoltage = $adapter;
}

my $product = $joltDifferences{1} * $joltDifferences{3};
print "$product\n";
