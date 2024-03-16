use strict;
use warnings;

my %cubes;
open(my $fh, '<', 'input.txt') or die $!;
while (my $line = <$fh>) {
    chomp $line;
    my @coords = split(',', $line);
    my $x = $coords[0];
    my $y = $coords[1];
    my $z = $coords[2];
    $cubes{"$x,$y,$z"} = 1;
}
close($fh);

my $surfaceArea = 0;
foreach my $cube (keys %cubes) {
    $surfaceArea += calculateExposedSides($cube, \%cubes);
}

print "$surfaceArea\n";

sub calculateExposedSides {
    my ($p, $cubes) = @_;
    my @directions = (
        [1, 0, 0], [-1, 0, 0], # x directions
        [0, 1, 0], [0, -1, 0], # y directions
        [0, 0, 1], [0, 0, -1]  # z directions
    );

    my $exposedSides = 6;
    my @p_coords = split(',', $p);
    foreach my $dir (@directions) {
        my $adjacent = [$p_coords[0] + $dir->[0], $p_coords[1] + $dir->[1], $p_coords[2] + $dir->[2]];
        if ($cubes->{"$adjacent->[0],$adjacent->[1],$adjacent->[2]"}) {
            $exposedSides--;
        }
    }
    return $exposedSides;
}