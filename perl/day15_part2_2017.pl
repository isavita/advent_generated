
use strict;
use warnings;

open(my $fh, '<', 'input.txt') or die "Error reading file: $!";
my @lines = <$fh>;
close($fh);

my $genAStart = int($lines[0]);
my $genBStart = int($lines[1]);

my $genAFactor = 16807;
my $genBFactor = 48271;
my $modulus = 2147483647;

my $genA = $genAStart;
my $genB = $genBStart;
my $matches = 0;

for (my $i = 0; $i < 5000000; $i++) {
    # Generate next value for A that is a multiple of 4
    while (1) {
        $genA = ($genA * $genAFactor) % $modulus;
        last if $genA % 4 == 0;
    }

    # Generate next value for B that is a multiple of 8
    while (1) {
        $genB = ($genB * $genBFactor) % $modulus;
        last if $genB % 8 == 0;
    }

    if (($genA & 0xFFFF) == ($genB & 0xFFFF)) {
        $matches++;
    }
}

print "$matches\n";
