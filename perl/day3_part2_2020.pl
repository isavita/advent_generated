#!/usr/bin/perl

use strict;
use warnings;

open(my $fh, '<', 'input.txt') or die "Could not open file 'input.txt': $!";
my @lines = <$fh>;
chomp @lines;
close($fh);

my @slopes = (
    [1, 1],
    [3, 1],
    [5, 1],
    [7, 1],
    [1, 2]
);

my $product = 1;
foreach my $slope (@slopes) {
    my $tree_count = 0;
    my $pos = 0;
    for (my $i = 0; $i < @lines; $i += $slope->[1]) {
        if (substr($lines[$i], $pos, 1) eq '#') {
            $tree_count++;
        }
        $pos = ($pos + $slope->[0]) % length($lines[$i]);
    }
    $product *= $tree_count;
}

print "$product\n";