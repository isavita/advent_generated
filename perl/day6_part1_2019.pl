#!/usr/bin/perl

use strict;
use warnings;

sub countOrbits {
    my ($orbitMap, $start, $depth) = @_;
    my $orbits = $orbitMap->{$start};
    return $depth unless defined $orbits;
    my $count = $depth;
    foreach my $orbit (@$orbits) {
        $count += countOrbits($orbitMap, $orbit, $depth + 1);
    }
    return $count;
}

open my $fh, '<', 'input.txt' or die "Could not open file 'input.txt': $!";
my $data = do { local $/; <$fh> };
close $fh;

my @lines = split /\n/, $data;
my %orbitMap;
foreach my $line (@lines) {
    my ($center, $orbiter) = split /\)/, $line;
    push @{$orbitMap{$center}}, $orbiter;
}

my $totalOrbits = countOrbits(\%orbitMap, "COM", 0);
print "$totalOrbits\n";