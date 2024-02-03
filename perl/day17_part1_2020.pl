
use strict;
use warnings;

open(my $fh, '<', 'input.txt') or die "Cannot open file: $!";
my @initialState = map { chomp; $_ } <$fh>;
close($fh);

my %activeCubes;

for my $y (0..$#initialState) {
    my @line = split('', $initialState[$y]);
    for my $x (0..$#line) {
        if ($line[$x] eq '#') {
            $activeCubes{"$x,$y,0"} = 1;
        }
    }
}

for (my $cycle = 0; $cycle < 6; $cycle++) {
    %activeCubes = simulateCycle(\%activeCubes);
}

print scalar(keys %activeCubes) . "\n";

sub simulateCycle {
    my %activeCubes = %{$_[0]};
    my %newActiveCubes;
    my %neighborCounts;

    foreach my $coord (keys %activeCubes) {
        my ($x, $y, $z) = split(',', $coord);
        for my $dz (-1, 0, 1) {
            for my $dy (-1, 0, 1) {
                for my $dx (-1, 0, 1) {
                    next if $dz == 0 && $dy == 0 && $dx == 0;
                    my $neighbor = ($x + $dx) . "," . ($y + $dy) . "," . ($z + $dz);
                    $neighborCounts{$neighbor}++;
                }
            }
        }
    }

    foreach my $coord (keys %neighborCounts) {
        my $count = $neighborCounts{$coord};
        if ($count == 3 || ($count == 2 && $activeCubes{$coord})) {
            $newActiveCubes{$coord} = 1;
        }
    }

    return %newActiveCubes;
}
