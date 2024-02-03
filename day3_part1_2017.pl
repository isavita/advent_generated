
use strict;
use warnings;

open(my $fh, '<', 'input.txt') or die "Could not open file 'input.txt' $!";
my $data = <$fh>;
close($fh);

chomp($data);
my $target = int($data);

my $sideLength = int(sqrt($target) + 0.5);
$sideLength++ if $sideLength % 2 == 0;

my $maxValue = $sideLength * $sideLength;
my $stepsFromEdge = int(($sideLength - 1) / 2);
my $distanceToMiddle;

for (my $i = 0; $i < 4; $i++) {
    my $middlePoint = $maxValue - $stepsFromEdge - ($sideLength - 1) * $i;
    my $distance = abs($target - $middlePoint);
    if ($distance < $distanceToMiddle || $i == 0) {
        $distanceToMiddle = $distance;
    }
}

my $manhattanDistance = $stepsFromEdge + $distanceToMiddle;

print "$manhattanDistance\n";
