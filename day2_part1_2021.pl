
use strict;
use warnings;

open(my $fh, '<', 'input.txt') or die $!;
my $horizontalPosition = 0;
my $depth = 0;

while (my $line = <$fh>) {
    chomp $line;
    my @command = split(' ', $line);
    my $direction = $command[0];
    my $units = $command[1];

    if ($direction eq "forward") {
        $horizontalPosition += $units;
    } elsif ($direction eq "down") {
        $depth += $units;
    } elsif ($direction eq "up") {
        $depth -= $units;
    }
}

my $product = $horizontalPosition * $depth;
print "$product\n";
