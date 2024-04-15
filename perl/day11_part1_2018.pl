use strict;
use warnings;

# Read serial number from file
open my $fh, '<', 'input.txt' or die $!;
my $serial = <$fh>;
chomp $serial;
close $fh;

# Constants and grid initialization
my $gridSize = 300;
my @grid = map { [(0) x $gridSize] } (1..$gridSize);

# Populate the power level grid
for my $y (1..$gridSize) {
    for my $x (1..$gridSize) {
        my $rackID = $x + 10;
        my $powerLevel = $rackID * $y;
        $powerLevel += $serial;
        $powerLevel *= $rackID;
        $powerLevel = int($powerLevel / 100) % 10;
        $powerLevel -= 5;
        $grid[$y-1][$x-1] = $powerLevel;
    }
}

# Find the top-left coordinate of the 3x3 square with the highest total power
my $maxPower = -1e9; # Use a very small number
my ($maxX, $maxY);

for my $y (0..$gridSize-3) {
    for my $x (0..$gridSize-3) {
        my $totalPower = 0;
        for my $dy (0..2) {
            for my $dx (0..2) {
                $totalPower += $grid[$y + $dy][$x + $dx];
            }
        }
        if ($totalPower > $maxPower) {
            $maxPower = $totalPower;
            ($maxX, $maxY) = ($x + 1, $y + 1);
        }
    }
}

# Output the result
print "$maxX,$maxY\n";