
use strict;
use warnings;

open(my $fh, '<', 'input.txt') or die "Error opening file: $!";
my %points;
my @folds;
my $readingPoints = 1;

while (my $line = <$fh>) {
    chomp $line;
    if ($line eq "") {
        $readingPoints = 0;
        next;
    }
    if ($readingPoints) {
        my @coords = split(',', $line);
        my $x = $coords[0];
        my $y = $coords[1];
        $points{"$x,$y"} = 1;
    } else {
        push @folds, $line;
    }
}

close($fh);

my $fold = (split(' ', $folds[0]))[2];
my ($axis, $value) = split('=', $fold);
my %newPoints;

if ($axis eq "x") {
    foreach my $point (keys %points) {
        my ($x, $y) = split(',', $point);
        if ($x > $value) {
            $x = 2*$value - $x;
        }
        $newPoints{"$x,$y"} = 1;
    }
} else {
    foreach my $point (keys %points) {
        my ($x, $y) = split(',', $point);
        if ($y > $value) {
            $y = 2*$value - $y;
        }
        $newPoints{"$x,$y"} = 1;
    }
}

print scalar keys %newPoints;
