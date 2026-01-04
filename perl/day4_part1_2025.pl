#!/usr/bin/perl
use strict;
use warnings;

open my $fh, '<', 'input.txt' or die $!;
my @grid = map { chomp; $_ } grep { $_ ne '' } <$fh>;
close $fh;

my $rows = @grid;
die "Empty grid\n" unless $rows;
my $cols = length $grid[0];
my @dirs = (-1,-1, 0,-1, 1,-1, -1,0, 1,0, -1,1, 0,1, 1,1);
my $count = 0;

for my $y (0..$rows-1) {
    for my $x (0..$cols-1) {
        next unless substr($grid[$y], $x, 1) eq '@';
        my $neighbors = 0;
        for (my $i = 0; $i < @dirs; $i += 2) {
            my $nx = $x + $dirs[$i];
            my $ny = $y + $dirs[$i+1];
            next if $nx < 0 || $nx >= $cols || $ny < 0 || $ny >= $rows;
            $neighbors++ if substr($grid[$ny], $nx, 1) eq '@';
        }
        $count++ if $neighbors < 4;
    }
}

print "Number of accessible rolls of paper: $count\n";