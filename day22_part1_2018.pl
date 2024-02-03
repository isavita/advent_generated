
use strict;
use warnings;

my $filename = 'input.txt';
open(my $fh, '<', $filename) or die "Could not open file '$filename' $!";
my $data = do { local $/; <$fh> };

my ($depth, $target) = parseInput($data);
my $cave = makeCaveSystem($depth, $target);
my $riskLevel = calculateRiskLevel($cave, $target);
print "Total Risk Level: $riskLevel\n";

sub parseInput {
    my $data = shift;
    my @lines = split("\n", $data);
    my $depth = (split(" ", $lines[0]))[1];
    my $coords = (split(" ", $lines[1]))[1];
    my @parts = split(",", $coords);
    my $x = $parts[0];
    my $y = $parts[1];
    return ($depth, [$x, $y]);
}

sub makeCaveSystem {
    my ($depth, $target) = @_;
    my @cave;
    for my $y (0..$target->[1]) {
        for my $x (0..$target->[0]) {
            my $geologicIndex;
            if ($x == 0 && $y == 0 || $x == $target->[0] && $y == $target->[1]) {
                $geologicIndex = 0;
            } elsif ($y == 0) {
                $geologicIndex = $x * 16807;
            } elsif ($x == 0) {
                $geologicIndex = $y * 48271;
            } else {
                $geologicIndex = $cave[$y][$x-1] * $cave[$y-1][$x];
            }
            $cave[$y][$x] = ($geologicIndex + $depth) % 20183;
        }
    }
    return \@cave;
}

sub calculateRiskLevel {
    my ($cave, $target) = @_;
    my $riskLevel = 0;
    for my $y (0..$target->[1]) {
        for my $x (0..$target->[0]) {
            $riskLevel += $cave->[$y][$x] % 3;
        }
    }
    return $riskLevel;
}
