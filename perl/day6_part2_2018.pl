
use strict;
use warnings;

# Read input from file
open(my $fh, '<', 'input.txt') or die $!; 
my $content = do { local $/; <$fh> };

# Parse coordinates
my @coordinates = parseCoordinates($content);

# Find the region size
my $regionSize = findRegionSize(\@coordinates, 10000);

# Print the result
print "$regionSize\n";

sub parseCoordinates {
    my ($input) = @_;
    my @lines = split("\n", $input);
    my @coordinates;

    foreach my $line (@lines) {
        my ($x, $y) = split(", ", $line);
        push @coordinates, { x => $x, y => $y };
    }

    return @coordinates;
}

sub findRegionSize {
    my ($coordinates, $maxDistance) = @_;
    my ($minX, $minY, $maxX, $maxY) = findBoundingBox($coordinates);
    my $regionSize = 0;

    for my $x ($minX..$maxX) {
        for my $y ($minY..$maxY) {
            my $totalDistance = 0;

            foreach my $c (@$coordinates) {
                $totalDistance += manhattanDistance($x, $y, $c->{x}, $c->{y});
            }

            if ($totalDistance < $maxDistance) {
                $regionSize++;
            }
        }
    }

    return $regionSize;
}

sub findBoundingBox {
    my ($coordinates) = @_;
    my ($minX, $minY) = (999999, 999999);
    my ($maxX, $maxY) = (-999999, -999999);

    foreach my $c (@$coordinates) {
        $minX = $c->{x} if $c->{x} < $minX;
        $minY = $c->{y} if $c->{y} < $minY;
        $maxX = $c->{x} if $c->{x} > $maxX;
        $maxY = $c->{y} if $c->{y} > $maxY;
    }

    return ($minX, $minY, $maxX, $maxY);
}

sub manhattanDistance {
    my ($x1, $y1, $x2, $y2) = @_;
    return abs($x1 - $x2) + abs($y1 - $y2);
}
