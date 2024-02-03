
use strict;
use warnings;

open(my $fh, '<', 'input.txt') or die $!;
my @positions;
while (my $line = <$fh>) {
    chomp $line;
    my @numbers = split(',', $line);
    foreach my $num_str (@numbers) {
        my $num = int($num_str);
        push @positions, $num;
    }
}
close($fh);

@positions = sort {$a <=> $b} @positions;

my $min_fuel = int(~0 >> 1);
foreach my $i ($positions[0]..$positions[$#positions]) {
    my $fuel = 0;
    foreach my $pos (@positions) {
        $fuel += calculateFuel($pos, $i);
    }
    if ($fuel < $min_fuel) {
        $min_fuel = $fuel;
    }
}
print "$min_fuel\n";

sub calculateFuel {
    my ($currentPosition, $newPosition) = @_;
    return abs($currentPosition - $newPosition);
}

sub abs {
    my $n = shift;
    if ($n < 0) {
        return -$n;
    }
    return $n;
}
