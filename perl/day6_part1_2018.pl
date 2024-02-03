
use strict;
use warnings;

open(my $fh, '<', 'input.txt') or die "Cannot open file: $!";
my @points;
my ($maxX, $maxY) = (0, 0);

while (my $line = <$fh>) {
    chomp $line;
    my @coords = split(', ', $line);
    my $x = $coords[0];
    my $y = $coords[1];
    $maxX = $x if $x > $maxX;
    $maxY = $y if $y > $maxY;
    push @points, [$x, $y];
}

my @grid;
for (my $i = 0; $i <= $maxX + 1; $i++) {
    for (my $j = 0; $j <= $maxY + 1; $j++) {
        $grid[$i][$j] = 0;
    }
}

my @areas = (0) x scalar @points;
my @infinite = (0) x scalar @points;

for my $i (0..$maxX + 1) {
    for my $j (0..$maxY + 1) {
        my $minDist = $maxX + $maxY;
        for my $k (0..$#points) {
            my $dist = abs($points[$k][0] - $i) + abs($points[$k][1] - $j);
            if ($dist < $minDist) {
                $minDist = $dist;
                $grid[$i][$j] = $k;
            } elsif ($dist == $minDist) {
                $grid[$i][$j] = -1;
            }
        }
        if ($grid[$i][$j] != -1) {
            if ($i == 0 || $j == 0 || $i == $maxX + 1 || $j == $maxY + 1) {
                $infinite[$grid[$i][$j]] = 1;
            }
            $areas[$grid[$i][$j]]++;
        }
    }
}

my $maxArea = 0;
for my $i (0..$#areas) {
    if (!$infinite[$i] && $areas[$i] > $maxArea) {
        $maxArea = $areas[$i];
    }
}

print "$maxArea\n";

sub abs {
    my $x = shift;
    return $x < 0 ? -$x : $x;
}
