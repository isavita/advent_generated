
use strict;
use warnings;

open my $fh, '<', 'input.txt' or die $!;

my @numbers;
while (my $line = <$fh>) {
    chomp $line;
    my @sides = split ' ', $line;
    my @row;
    foreach my $side (@sides) {
        my $num = int($side);
        push @row, $num;
    }
    push @numbers, \@row;
}

my $validTriangles = 0;
for (my $i = 0; $i < scalar @{$numbers[0]}; $i++) {
    for (my $j = 0; $j < scalar @numbers; $j += 3) {
        if ($j + 2 < scalar @numbers && isValidTriangle($numbers[$j][$i], $numbers[$j+1][$i], $numbers[$j+2][$i])) {
            $validTriangles++;
        }
    }
}

print "$validTriangles\n";

sub isValidTriangle {
    my ($a, $b, $c) = @_;
    return $a + $b > $c && $a + $c > $b && $b + $c > $a;
}
