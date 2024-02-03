
use strict;
use warnings;

open(my $fh, '<', 'input.txt') or die $!;

my @heightmap;
while (my $line = <$fh>) {
    chomp $line;
    my @row = split('', $line);
    my @heights;
    foreach my $char (@row) {
        my $height = int($char);
        push @heights, $height;
    }
    push @heightmap, \@heights;
}

close($fh);

my $totalRiskLevel = 0;
foreach my $y (0 .. $#heightmap) {
    foreach my $x (0 .. $#{$heightmap[$y]}) {
        if (isLowPoint(\@heightmap, $x, $y)) {
            $totalRiskLevel += 1 + $heightmap[$y][$x];
        }
    }
}

print "$totalRiskLevel\n";

sub isLowPoint {
    my ($heightmap, $x, $y) = @_;
    my $height = $heightmap->[$y][$x];
    if ($x > 0 && $heightmap->[$y][$x-1] <= $height) {
        return 0;
    }
    if ($x < $#{$heightmap->[$y]} && $heightmap->[$y][$x+1] <= $height) {
        return 0;
    }
    if ($y > 0 && $heightmap->[$y-1][$x] <= $height) {
        return 0;
    }
    if ($y < $#heightmap && $heightmap->[$y+1][$x] <= $height) {
        return 0;
    }
    return 1;
}
