
use strict;
use warnings;

open(my $fh, '<', "input.txt") or die $!;

my $total = 0;
while (my $line = <$fh>) {
    chomp $line;
    my @dimensions = split('x', $line);
    if (@dimensions != 3) {
        die "Invalid input format";
    }

    my $l = $dimensions[0];
    my $w = $dimensions[1];
    my $h = $dimensions[2];

    my $side1 = $l * $w;
    my $side2 = $w * $h;
    my $side3 = $h * $l;

    my $smallest = min($side1, $side2, $side3);
    $total += 2*$side1 + 2*$side2 + 2*$side3 + $smallest;
}

close($fh);

print "$total\n";

sub min {
    my $minVal = $_[0];
    foreach my $val (@_[1..$#_]) {
        if ($val < $minVal) {
            $minVal = $val;
        }
    }
    return $minVal;
}
