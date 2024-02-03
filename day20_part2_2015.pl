
use strict;
use warnings;

open(my $fh, '<', 'input.txt') or die $!;
my $input = <$fh>;
close($fh);

chomp($input);
my $target = int($input) / 11;

my @houses = (0) x ($target + 1);

for (my $elf = 1; $elf <= $target; $elf++) {
    for (my $house = $elf; $house <= $elf * 50 && $house <= $target; $house += $elf) {
        $houses[$house] += $elf;
    }
}

for my $houseNumber (0..$#houses) {
    if ($houses[$houseNumber] >= $target) {
        print "$houseNumber\n";
        last;
    }
}
