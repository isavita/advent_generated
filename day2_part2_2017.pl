
use strict;
use warnings;

open(my $fh, '<', 'input.txt') or die "Cannot open file: $!";

my $sum = 0;

while (my $line = <$fh>) {
    chomp $line;
    my @nums = split(' ', $line);

    for my $i (0..$#nums) {
        for my $j (0..$#nums) {
            if ($i != $j && $nums[$i] % $nums[$j] == 0) {
                $sum += $nums[$i] / $nums[$j];
            }
        }
    }
}

close($fh);

print $sum . "\n";
