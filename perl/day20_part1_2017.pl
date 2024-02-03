#!/usr/bin/perl

use strict;
use warnings;

sub abs {
    my $x = shift;
    if ($x < 0) {
        return -$x;
    } else {
        return $x;
    }
}

sub manhattan {
    my @x = @{$_[0]};
    return abs($x[0]) + abs($x[1]) + abs($x[2]);
}

open(my $fh, "<", "input.txt") or die "Cannot open file: $!";
my @particles;

while (my $line = <$fh>) {
    chomp($line);
    my @parts = split(", ", $line);

    my %p;
    foreach my $i (0..2) {
        my @coords = split(",", substr($parts[$i], 3, -1));
        foreach my $j (0..2) {
            my $num = int($coords[$j]);
            if ($i == 0) {
                $p{p}[$j] = $num;
            } elsif ($i == 1) {
                $p{v}[$j] = $num;
            } elsif ($i == 2) {
                $p{a}[$j] = $num;
            }
        }
    }
    push @particles, \%p;
}

my $closestParticle;
my $minAccel = 9223372036854775807;
my $minVelocity = 9223372036854775807;
my $minPosition = 9223372036854775807;

foreach my $i (0..$#particles) {
    my $particle = $particles[$i];
    my $accel = manhattan($particle->{a});
    my $velocity = manhattan($particle->{v});
    my $position = manhattan($particle->{p});

    if ($accel < $minAccel || ($accel == $minAccel && $velocity < $minVelocity) ||
        ($accel == $minAccel && $velocity == $minVelocity && $position < $minPosition)) {
        $minAccel = $accel;
        $minVelocity = $velocity;
        $minPosition = $position;
        $closestParticle = $i;
    }
}

print "$closestParticle\n";