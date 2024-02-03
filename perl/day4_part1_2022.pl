
use strict;
use warnings;

sub parseRange {
    my ($r) = @_;
    my ($start, $end) = split('-', $r);
    return ($start, $end);
}

open(my $fh, '<', 'input.txt') or die "Error opening file: $!";
my $count = 0;

while (my $line = <$fh>) {
    chomp($line);
    my @ranges = split(',', $line);
    if (@ranges != 2) {
        next;
    }
    my ($start1, $end1) = parseRange($ranges[0]);
    my ($start2, $end2) = parseRange($ranges[1]);

    if (($start1 <= $start2 && $end1 >= $end2) || ($start2 <= $start1 && $end2 >= $end1)) {
        $count++;
    }
}

close($fh) or die "Error closing file: $!";

print "$count\n";
