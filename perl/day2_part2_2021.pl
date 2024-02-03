
use strict;
use warnings;

open(my $fh, '<', 'input.txt') or die $!;

my $horizontalPosition = 0;
my $depth = 0;
my $aim = 0;

while (my $line = <$fh>) {
    chomp($line);
    my @command = split(' ', $line);
    my $direction = $command[0];
    my $units = $command[1];

    if ($direction eq "forward") {
        $horizontalPosition += $units;
        $depth += $aim * $units;
    } elsif ($direction eq "down") {
        $aim += $units;
    } elsif ($direction eq "up") {
        $aim -= $units;
    }
}

my $product = $horizontalPosition * $depth;
print "$product\n";
