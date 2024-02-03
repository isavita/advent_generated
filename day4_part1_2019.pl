
use strict;
use warnings;

open(my $fh, '<', 'input.txt') or die $!;
my $rangeStr = <$fh>;
close($fh);

my ($start, $end) = split('-', $rangeStr);

my $count = 0;
for (my $i = $start; $i <= $end; $i++) {
    my $s = $i;
    if (hasDoubleAndIncreasingDigits($s)) {
        $count++;
    }
}

print "$count\n";

sub hasDoubleAndIncreasingDigits {
    my $s = shift;
    my $hasDouble = 0;
    for (my $i = 0; $i < length($s) - 1; $i++) {
        if (substr($s, $i, 1) == substr($s, $i + 1, 1)) {
            $hasDouble = 1;
        }
        if (substr($s, $i, 1) > substr($s, $i + 1, 1)) {
            return 0;
        }
    }
    return $hasDouble;
}
