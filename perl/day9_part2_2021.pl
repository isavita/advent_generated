
use strict;
use warnings;
use List::Util qw(reduce);

open(my $fh, '<', 'input.txt') or die $!;

my @heightmap;
while (my $line = <$fh>) {
    chomp $line;
    my @row = map { $_ + 0 } split('', $line);
    push @heightmap, \@row;
}

my @basinSizes;
my %visited;

for my $y (0 .. $#heightmap) {
    for my $x (0 .. $#{$heightmap[$y]}) {
        if (isLowPoint(\@heightmap, $x, $y)) {
            my $size = exploreBasin(\@heightmap, $x, $y, \%visited);
            push @basinSizes, $size;
        }
    }
}

@basinSizes = sort { $b <=> $a } @basinSizes;
my $result = reduce { $a * $b } @basinSizes[0..2];
print "$result\n";

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

sub exploreBasin {
    my ($heightmap, $x, $y, $visited) = @_;
    if ($visited->{"$x,$y"} || $heightmap->[$y][$x] == 9) {
        return 0;
    }
    $visited->{"$x,$y"} = 1;
    my $size = 1;

    my @directions = ([0, -1], [-1, 0], [0, 1], [1, 0]);
    for my $dir (@directions) {
        my ($newX, $newY) = ($x + $dir->[0], $y + $dir->[1]);
        if ($newX >= 0 && $newX < @{$heightmap->[0]} && $newY >= 0 && $newY < @heightmap) {
            $size += exploreBasin($heightmap, $newX, $newY, $visited);
        }
    }
    return $size;
}
