
use strict;
use warnings;

open(my $fh, '<', 'input.txt') or die "Cannot open file: $!";
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
for (my $i = $positions[0]; $i <= $positions[$#positions]; $i++) {
    my $fuel = 0;
    foreach my $pos (@positions) {
        $fuel += calculateNewFuel($pos, $i);
    }
    if ($fuel < $min_fuel) {
        $min_fuel = $fuel;
    }
}
print "$min_fuel\n";

sub calculateNewFuel {
    my ($currentPosition, $newPosition) = @_;
    my $diff = abs($currentPosition - $newPosition);
    return ($diff * ($diff + 1)) / 2;
}

sub abs {
    my $n = shift;
    return $n < 0 ? -$n : $n;
}
