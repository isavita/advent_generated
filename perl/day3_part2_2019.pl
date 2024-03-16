#!/usr/bin/perl

use strict;
use warnings;

my $input_file = "input.txt";
open(my $fh, '<', $input_file) or die "Could not open file '$input_file' $!";

my @lines = <$fh>;
chomp(@lines);

my %wire1 = getPointsWithSteps($lines[0]);
my %wire2 = getPointsWithSteps($lines[1]);

my $min_steps = ~0 >> 1;
foreach my $p (keys %wire1) {
    if (exists $wire2{$p}) {
        my $total_steps = $wire1{$p} + $wire2{$p};
        $min_steps = $total_steps if $total_steps < $min_steps;
    }
}

print "$min_steps\n";

sub getPointsWithSteps {
    my ($path) = @_;
    my %points;
    my @path_moves = split(',', $path);
    my $current = { X => 0, Y => 0 };
    my $steps = 0;

    foreach my $move (@path_moves) {
        my $dir = substr($move, 0, 1);
        my $dist = substr($move, 1);

        for (my $i = 0; $i < $dist; $i++) {
            $steps++;
            if ($dir eq 'U') {
                $current->{Y}++;
            } elsif ($dir eq 'D') {
                $current->{Y}--;
            } elsif ($dir eq 'L') {
                $current->{X}--;
            } elsif ($dir eq 'R') {
                $current->{X}++;
            }

            my $point = "$current->{X},$current->{Y}";
            if (!exists $points{$point}) {
                $points{$point} = $steps;
            }
        }
    }

    return %points;
}