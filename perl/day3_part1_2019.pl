#!/usr/bin/perl

use strict;
use warnings;

sub get_points {
    my $path = shift;
    my %points;
    my @current = (0, 0);
    for my $move (split(",", $path)) {
        my $dir = substr($move, 0, 1);
        my $steps = substr($move, 1);
        for (my $i = 0; $i < $steps; $i++) {
            if ($dir eq 'U') {
                $current[1]++;
            } elsif ($dir eq 'D') {
                $current[1]--;
            } elsif ($dir eq 'L') {
                $current[0]--;
            } elsif ($dir eq 'R') {
                $current[0]++;
            }
            $points{"$current[0],$current[1]"} = 1;
        }
    }
    return %points;
}

open(my $fh, '<', "input.txt") or die "Cannot open file: $!";
my @lines = <$fh>;
close($fh);

my %wire1 = get_points($lines[0]);
my %wire2 = get_points($lines[1]);

my %intersections;
foreach my $p1 (keys %wire1) {
    if ($wire2{$p1}) {
        $intersections{$p1} = 1;
    }
}

my $min_distance = 2**32;
foreach my $p (keys %intersections) {
    my @coords = split(",", $p);
    my $distance = abs($coords[0]) + abs($coords[1]);
    if ($distance < $min_distance) {
        $min_distance = $distance;
    }
}

print "$min_distance\n";

sub abs {
    my $x = shift;
    return $x < 0 ? -$x : $x;
}