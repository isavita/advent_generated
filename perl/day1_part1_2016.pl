
use strict;
use warnings;

open(my $fh, '<', 'input.txt') or die $!;

my @instructions;
while (my $line = <$fh>) {
    chomp $line;
    @instructions = split(', ', $line);
}

close($fh);

my $pos = { x => 0, y => 0, dirIndex => 0 };
my @directions = ([0, 1], [1, 0], [0, -1], [-1, 0]);

foreach my $instruction (@instructions) {
    my $turn = substr($instruction, 0, 1);
    my $blocks = substr($instruction, 1);

    if ($turn eq "R") {
        $pos->{dirIndex} = ($pos->{dirIndex} + 1) % 4;
    }
    else {
        $pos->{dirIndex} = ($pos->{dirIndex} - 1 + 4) % 4;
    }

    $pos->{x} += $directions[$pos->{dirIndex}][0] * $blocks;
    $pos->{y} += $directions[$pos->{dirIndex}][1] * $blocks;
}

print abs($pos->{x}) + abs($pos->{y}) . "\n";

sub abs {
    my $x = shift;
    return $x < 0 ? -$x : $x;
}
