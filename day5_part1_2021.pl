
use strict;
use warnings;

my %grid;

open(my $fh, '<', 'input.txt') or die "Cannot open file: $!";

while (my $line = <$fh>) {
    chomp $line;
    my @coords = split(' -> ', $line);
    my @startCoords = split(',', $coords[0]);
    my @endCoords = split(',', $coords[1]);

    my $x1 = $startCoords[0];
    my $y1 = $startCoords[1];
    my $x2 = $endCoords[0];
    my $y2 = $endCoords[1];

    if ($x1 == $x2) {
        if ($y1 > $y2) {
            ($y1, $y2) = ($y2, $y1);
        }
        for my $y ($y1..$y2) {
            $grid{"$x1,$y"}++;
        }
    } elsif ($y1 == $y2) {
        if ($x1 > $x2) {
            ($x1, $x2) = ($x2, $x1);
        }
        for my $x ($x1..$x2) {
            $grid{"$x,$y1"}++;
        }
    }
}

my $overlapCount = 0;
foreach my $v (values %grid) {
    if ($v > 1) {
        $overlapCount++;
    }
}

print "$overlapCount\n";
