
use strict;
use warnings;

open(my $fh, '<', 'input.txt') or die $!;

my $validTriangles = 0;
while (my $line = <$fh>) {
    chomp($line);
    my @sides = split(' ', $line);
    if (@sides != 3) {
        print "Invalid input format\n";
    } else {
        my ($a, $b, $c) = @sides;
        if (isValidTriangle($a, $b, $c)) {
            $validTriangles++;
        }
    }
}

print "$validTriangles\n";

sub isValidTriangle {
    my ($a, $b, $c) = @_;
    return $a + $b > $c && $a + $c > $b && $b + $c > $a;
}
