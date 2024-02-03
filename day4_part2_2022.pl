
use strict;
use warnings;

open(my $fh, '<', "input.txt") or die "Cannot open file: $!";
my $count = 0;

while (my $line = <$fh>) {
    chomp $line;
    my @pair = split(',', $line);

    # Extract ranges
    my @left = parseRange($pair[0]);
    my @right = parseRange($pair[1]);

    # Check if ranges overlap
    if ($left[0] <= $right[1] && $left[1] >= $right[0]) {
        $count++;
    }
}

print "$count\n";

sub parseRange {
    my $s = shift;
    my @split = split('-', $s);
    my $start = $split[0];
    my $end = $split[1];
    return ($start, $end);
}
