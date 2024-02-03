
use strict;
use warnings;
use List::Util qw(min);

open(my $fh, '<', 'input.txt') or die $!;

my $totalRibbon = 0;
while (my $line = <$fh>) {
    chomp $line;
    my @dimensions = split('x', $line);
    if (@dimensions != 3) {
        die "Invalid input format\n";
    }

    my ($l, $w, $h) = @dimensions;

    # Calculate ribbon for the bow
    my $bow = $l * $w * $h;

    # Calculate ribbon for wrapping (smallest perimeter)
    my @sides = sort { $a <=> $b } ($l, $w, $h);
    my $wrap = 2*$sides[0] + 2*$sides[1];

    $totalRibbon += $bow + $wrap;
}

close($fh);

print "$totalRibbon\n";
