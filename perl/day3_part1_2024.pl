
#!/usr/bin/perl

use strict;
use warnings;

my $totalSum = 0;

open(my $fh, '<', 'input.txt') or die "Could not open file: $!";
my $input = do { local $/; <$fh> };
close $fh;


while ($input =~ /mul\((\d+),(\d+)\)/g) {
    $totalSum += $1 * $2;
}

print $totalSum . "\n";
